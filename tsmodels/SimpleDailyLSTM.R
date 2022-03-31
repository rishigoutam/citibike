# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)
#Parquet Processing
library(reticulate)
library(dplyr) #redundant but eh
# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
library(lubridate)
# Visualization
library(cowplot)
library(patchwork)
library(gganimate)
# Preprocessing
library(recipes)
library(sjmisc)
# Sampling / Accuracy
library(rsample)
library(yardstick) 
# Modeling
library(keras)


data <- read_csv('../data/NY_2019.csv')
data <- data[-1]
weather <- read_csv('../data/weather/GHCN-Daily-Cleaned.csv')
weather <- weather[-1]
#we try summarize_at day(data$starttime)
data$day <- date(data$starttime)

daily <- data %>% 
  count(day) %>% ungroup() %>%
  tk_tbl() %>% as_tbl_time(index = day)


p1 <- daily %>% ggplot(aes(day, n)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "2019 (Full Year Set)"
  )

p2 <- daily  %>% filter_time("start" ~ "2019-01") %>%
  ggplot(aes(day, n)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Jan 2019 (First Month Zoomed to show Season)"
  )

p3 <- daily  %>% filter_time("2019-02" ~ "2019-02") %>%
  ggplot(aes(day, n)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Feb 2019 (Second Month Zoomed to show Season)"
  )
p1/ p2 / p3



#looks like the period is approximately 7 days, and different months,\
#have different baseline periods. 
#We can do batch forecasting of 7 days in advance if the autocorrelation
# is beyond 7 days. 

#We check if autocrrelation is significant ourside of 7 days
daily %>% select(n) %>% acf() 
#From the acf function we see that it is 

#We want to get the autocorrelation data itself
tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}

#need autocorr in excess of .5 beyond the next week (24 hr lag mark)
max_lag <-7 
daily %>%
  tidy_acf(2, lags = 0:max_lag)
#We see all the ACF values within our prediction window are sigificant (>.5)
#And that beyond our prediciton window they stay significant for a while. 

#Since we have HIGH autocorrelation, beyond .5, after the 1 week mark
# we can use the high autocorrelation LSTM model 


optimal_lag_setting <- daily %>%
  tidy_acf(2, lags = 2:max_lag) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
optimal_lag_setting
#Optimal Lag is at a week. 



#Backtesting
#In time series, we want to preserve the time component
#when splitting up data to train on, and to cross-validate on
#instead of randomly splitting up all of the rows and etc. 

#Given yearly data say we have a sampling plan
#we have in total 365 days. 
#take 20 days as training, 11 as test, and we have to move our origin up 31 days each time
#so our skip is 31 days or a month.
#we take a skip span of 1 month, in order to divide entire data set into 11 sets

periods_train <- 24 #train 24 days
periods_test <- 12 #test 14 days
skip_span <- 28 #skip 28 days 

rolling_origin_resamples <- rolling_origin(
  daily,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)
rolling_origin_resamples




#Visualizing the backtesting
#functions for visualizing the resamples
# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = day) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = day, y = n, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  #Got segmentation plot to work 
  if (expand_y_axis) {
    
    daily_time_summary <- daily %>% 
      tk_index()  %>% 
      tk_get_timeseries_summary()
    
    g <- g + scale_x_date(date_labels = "%b-%d-%Y", limits = c(daily_time_summary$start, daily_time_summary$end))
      
      #scale_x_datetime( limits = c(daily_time_summary$start, daily_time_summary$end))
  }
  
  
  
  return(g)
}


rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")










# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}
#Plotting our Backtesting strategy
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")


rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Zoomed In")






#Using the keras Stateful LSTM Model 

#first do model on one sample, then we scale model to other samples
#this allows us to check if our model is even working at all
#then crossvalidate it when applying it to the other years



#_________________________________Single LSTM 
split <-rolling_origin_resamples$splits[[11]]
split_id <- rolling_origin_resamples$id[[11]]

plot_split(split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle("Split")


df_trn <- training(split)
df_tst <- testing(split)


#add column from sjmisc adds column to the end 
df <- bind_rows( 
  df_trn %>% add_column(key="training"), 
  df_tst %>% add_columns(key='testing') 
) %>%
  as_tbl_time(index=day) 





#preprocessing--LSTM wants centered and normalized and scaled
rec_obj <- recipe(n ~ ., df) %>% 
  step_sqrt(n) %>% 
  step_center(n) %>% 
  step_scale(n) %>%
  prep()

df_processed_tbl <- bake(rec_obj, df) #counts are transformed

#We record centering and scaling history in order to inver the 
#centering and scaling transformations after the model
center_history <- rec_obj$steps[[2]]$means["n"]
scale_history  <- rec_obj$steps[[3]]$sds["n"]
c("center" = center_history, "scale" = scale_history) #print histories


#____lstm_____
#model inputs: Batch splitting
#
lag_setting <- nrow(df_tst) #Prediction Window (How Far Ahead are we predicting, 
#thats a week) Highest Autocorrelation value
batch_size <- 12  #Batch size is the period giving best autocorr
#gcf()

train_length <- nrow(df_trn) #Training Window
tsteps <- 12 #number of training steps, we have 12 days, so we use 12 day lags
epochs <- 300 #need to adjust this number later based on bias/variance tradeoff
#ie, earlystopping to prevent overfitting. 




#_____Tensorize inputs 
# Training Set
lag_train_tbl <- df_processed_tbl %>%
  mutate(value_lag = lag(n, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)
x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), tsteps, 1))
y_train_vec <- lag_train_tbl$n
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))


# Testing Set
lag_test_tbl <- df_processed_tbl %>%
  mutate(
    value_lag = lag(n, n = lag_setting)
  ) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "testing")
x_test_vec <- lag_test_tbl$value_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), tsteps, 1))
y_test_vec <- lag_test_tbl$n
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))



#LSTMPLAN
#NEED THIS FOR TOTALMODEL
#lag_setting = nrow(df_tst)
#batch_size = 12
#train_length = nrow(df_train)
#tsteps = 12
#epochs = 300




#___________LSTM Model 
model <- keras_model_sequential()
model %>%
  layer_lstm(units            = 50, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)
model %>% 
  compile(loss = 'mae', optimizer = optimizer_rmsprop())
#we use mean absolute error as our loss function since it is most robust to outliers
#there are lots of outliers in our data. 

model



#Fitting the model
for (i in 1:1) {
  hist <- model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 300, 
                verbose    = 1,
                shuffle    = FALSE)
  #model %>% reset_states()
  cat("Epoch: ", i)
  
}

plot(hist)
#Predicting Using the 12-day LSTM Model
#x_test_arr, is our entire test-set tensor(with all the observations)
#using scale_history and center_history, we retransform and square the output
#to get the the final true count value 

# Make Predictions
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1] 
# Retransform values
pred_tbl <- tibble(
  index   = lag_test_tbl$day,
  value   = (pred_out * scale_history + center_history)^2
) 
# Combine actual data with predictions
tbl_1 <- df_trn %>%
  add_column(key = "actual")
tbl_1 <-tbl_1 %>% rename(index=day,value=n)
tbl_2 <- df_tst %>%
  add_column(key = "actual")
tbl_2 <- tbl_2 %>% rename(index=day,value=n)
tbl_3 <- pred_tbl %>%
  add_column(key = "predict")
# Create time_bind_rows() to solve dplyr issue
time_bind_rows <- function(data_1, data_2, index) {
  index_expr <- enquo(index)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}
ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, index = index) %>%
  arrange(key, index) %>%
  mutate(key = as_factor(key))
ret

#now we have the actual, the test actual, and the prediction for all 


#Assess Performance of LSTM on single Slice
calc_rmse <- function(prediction_tbl) {
  
  rmse_calculation <- function(data) {
    data %>%
      spread(key = key, value = value) %>%
      select(-index) %>%
      filter(!is.na(predict)) %>%
      rename(
        truth    = actual,
        estimate = predict
      ) %>%
      rmse(truth, estimate)
  }
  
  safe_rmse <- possibly(rmse_calculation, otherwise = NA)
  
  safe_rmse(prediction_tbl)
  
}


#gives us the rmse of single sample test. 
#however, note that this doesnt really show us anything. 
calc_rmse(ret)

#Visualizing___________
# Setup single plot function
plot_prediction <- function(data, id_s, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- calc_rmse(ret)$`.estimate`
  
  g <- data %>%
    ggplot(aes(index, value, color = key)) +
    geom_line(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id_s}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )
  
  return(g)
}
id #this is natively saved to something else, so internal functions
#get confused. 

ret %>% 
  plot_prediction(id_s = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")





