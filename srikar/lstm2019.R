# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)
# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
library(lubridate)
# Visualization
library(cowplot)
library(patchwork)
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
#we try summarize_at hour(data$starttime)
data$min <- round_date(data$starttime, unit="minute")
data$hour <- round_date(data$starttime, unit="hour")
data$day <- date(data$starttime)
data$week <- round_date(data$starttime, unit="week")


hourly <- data %>% 
  count(hour) %>% ungroup() %>%
  tk_tbl() %>% as_tbl_time(index = hour)

p1 <- hourly %>% ggplot(aes(hour, n)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "2019 (Full Year Set)"
  )

p2 <- hourly  %>% filter_time("start" ~ "2019-01") %>%
  ggplot(aes(hour, n)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Jan 2019 (First Month Zoomed to show Season)"
  )

p3 <- hourly  %>% filter_time("2019-02" ~ "2019-02") %>%
  ggplot(aes(hour, n)) +
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
hourly %>% select(n) %>% acf()
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

#need autocorr in excess of .5 beyond the next day (24 hr lag mark)
max_lag <-24 * 7 
hourly %>%
  tidy_acf(2, lags = 0:max_lag)



hourly %>% tidy_acf(2, lags = 0:max_lag) %>% 
  ggplot(aes(lag,acf)) + geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 24, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "1 Day Mark", x = 30, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF")


#Since we have HIGH autocorrelation, beyond .5, after the 1 day mark
# we can use the high autocorrelation LSTM model 


optimal_lag_setting <- hourly %>%
  tidy_acf(2, lags = 2:max_lag) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
optimal_lag_setting
#the optimal lag setting is 168hrs or 7 days. 







#Backtesting
#In time series, we want to preserve the time component
#when splitting up data to train on, and to cross-validate on
#instead of randomly splitting up all of the rows and etc. 

#Given yearly data say we have a sampling plan
#we have in total 365 days. 
#take 20 days as training, 11 as test, and we have to move our origin up 31 days each time
#so our skip is 31 days or a month.
#we take a skip span of 1 month, in order to divide entire data set into 11 sets

periods_train <- 24*24 #train 24 days
periods_test <- 24*7 #test 7 days
skip_span <- 24*30 #skip 14 days 

rolling_origin_resamples <- rolling_origin(
  hourly,
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
    as_tbl_time(index = hour) %>%
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
    ggplot(aes(x = hour, y = n, color = key)) +
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
    
    hourly_time_summary <- hourly %>% 
      tk_index()  %>% 
      tk_get_timeseries_summary()
    
    g <- g + scale_x_datetime( limits = c(hourly_time_summary$start, hourly_time_summary$end))
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
split <-rolling_origin_resamples$splits[[12]]
split_id <- rolling_origin_resamples$splits[[12]]

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
  as_tbl_time(index=hour) 





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
lag_setting <- nrow(df_tst)
batch_size <- 24 
train_length <- nrow(df_trn)
tsteps <- 1
epochs <- 200


#_____Tensorize inputs 
# Training Set
lag_train_tbl <- df_processed_tbl %>%
  mutate(value_lag = lag(n, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)
x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
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
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
y_test_vec <- lag_test_tbl$n
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))


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
  compile(loss = 'mae', optimizer = 'adam')
model



#Fitting the model
for (i in 1:epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

