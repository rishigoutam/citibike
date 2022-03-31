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
library(tsibble)
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
weather <- read_csv('../data/weather/GHCN-Daily-Cleaned.csv')
weather <- weather[-1]
#we try summarize_at day(data$starttime)
data$day <- date(data$starttime)

daily <- data %>% 
  count(day) %>% ungroup() %>%
  tk_tbl() %>% as_tbl_time(index = day)




weather <- weather %>% filter(year(ymd(DATE))=='2019') %>% select('DATE','PRCP','TAVG_F')
weather$DATE <- date(weather$DATE)

weather <- weather %>%
  tk_tbl() %>% as_tbl_time(index = DATE)
#cleanupspace
rm(data)

n1 <- daily %>% ggplot(aes(day, n)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "2019 Riders"
  )
t1 <- weather %>% ggplot(aes(DATE, TAVG_F)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "2019 AvTemp"
  )
p1 <- weather %>% ggplot(aes(DATE,PRCP)) +geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "2019 Percipitation"
  )

n1/t1/p1

n2 <- daily  %>% filter_time("start" ~ "2019-01") %>%
  ggplot(aes(day, n)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Jan 2019 (First Month Zoomed to show Season)"
  )

t2 <- weather %>% filter_time("start" ~ "2019-01") %>% ggplot(aes(DATE, TAVG_F)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Jan 2019 Temp"
  )

p2 <- weather %>% filter_time("start" ~ "2019-01")  %>%  ggplot(aes(DATE,PRCP)) + geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Jan 2019 Percipitation"
  )

n2/t2/p2


n2 <- daily  %>% filter_time("2019-03" ~ "2019-05") %>%
  ggplot(aes(day, n)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Early Mid 2019 Riders"
  )

t2 <- weather %>% filter_time("2019-03" ~ "2019-05") %>% ggplot(aes(DATE, TAVG_F)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Early Mid 2019 Temp"
  )

p2 <- weather %>%filter_time("2019-03" ~ "2019-05") %>%  ggplot(aes(DATE,PRCP)) + geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Early Mid 2019 Percipitation"
  )


n2/t2/p2

rm(n1,n2,p1,p2,t1,t2)
#Join
colnames(weather)[1] <- "day"
daily <- full_join(daily,weather)

daily
rm(weather)
#Backtesting
#In time series, we want to preserve the time component
#when splitting up data to train on, and to cross-validate on
#instead of randomly splitting up all of the rows and etc. 


#Timestep is in Days
periods_train = 24 
periods_test = 7
skip_span = 30 

rolling_origin_resamples <- rolling_origin(
  daily,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)
rolling_origin_resamples


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
    ggplot() + 
    geom_line(aes(x = day, y = n, color = key), size = size, alpha = alpha) +    #<-----change the y variable here, or the x variable, to change whats plotted
    theme_tq(base_size = base_size) +                                           #in our case, the y is number, n. but we can try PRCP, percipitation
    scale_color_tq() +
    labs(
      title    = glue("Ride Number Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  gt <- data_manipulated %>%
    ggplot() + 
    geom_line(aes(x = day, y = TAVG_F, color = key), size = size, alpha = alpha*.7) +    #<-----change the y variable here, or the x variable, to change whats plotted
    theme_tq(base_size = base_size) +                                           #in our case, the y is number, n. but we can try PRCP, percipitation
    scale_color_tq() +
    labs(
      title    = glue("Average Temperature (F) Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none")
  
  gp <- data_manipulated %>%
    ggplot() + 
    geom_line(aes(x = day, y = PRCP, color = key), size = size, alpha = alpha*.7) +    #<-----change the y variable here, or the x variable, to change whats plotted
    theme_tq(base_size = base_size) +                                           #in our case, the y is number, n. but we can try PRCP, percipitation
    scale_color_tq() +
    labs(
      title    = glue("Precipitation Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none")
  #Got segmentation plot to work 
  if (expand_y_axis) {
    
    daily_time_summary <- daily %>% 
      tk_index()  %>% 
      tk_get_timeseries_summary()
    
    g <- g + scale_x_date( limits = c(daily_time_summary$start, daily_time_summary$end))
    gt <- gt + scale_x_date( limits = c(daily_time_summary$start, daily_time_summary$end))
    gp <- gp + scale_x_date( limits = c(daily_time_summary$start, daily_time_summary$end))
  }
  
  return(g)
  #return(g/gt/gp)
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


#Plotting our Backtesting strategy (only plot one of the above when doing this, ie. return g)
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")


rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Zoomed In")



#Using the keras Stateful LSTM Model 
#Using the keras Stateful LSTM Model 

#first do model on one sample, then we scale model to other samples
#this allows us to check if our model is even working at all
#then crossvalidate it when applying it to the other years



# #_________________________________Single LSTM 
# split <-rolling_origin_resamples$splits[[11]]
# split_id <- rolling_origin_resamples$id[[11]]
# 
# plot_split(split, expand_y_axis = FALSE, size = 0.5) +
#   theme(legend.position = "bottom") +
#   ggtitle("Split")
# 
# 
# df_trn <- training(split)
# df_tst <- testing(split)
# 
# 
# #add column from sjmisc adds column to the end 
# df <- bind_rows( 
#   df_trn %>% add_column(key="training"), 
#   df_tst %>% add_columns(key='testing') 
# ) %>%
#   as_tbl_time(index=day) 
# df
# 
# 
# 
# #preprocessing--LSTM wants centered and normalized and scaled
# rec_obj <- recipe(n ~ ., df) %>% 
#   step_sqrt(n) %>% 
#   step_center(n) %>% 
#   step_scale(n) %>%
#   step_sqrt(PRCP) %>% 
#   step_center(PRCP) %>% 
#   step_scale(PRCP) %>%
#   step_sqrt(TAVG_F) %>% 
#   step_center(TAVG_F) %>% 
#   step_scale(TAVG_F) %>%
#   prep()
# 
# df_processed_tbl <- bake(rec_obj, df) #counts are transformed
# df_processed_tbl
# 
# 
# #We record centering and scaling history in order to inver the 
# #centering and scaling transformations after the model
# center_history <- rec_obj$steps[[2]]$means["n"]
# scale_history  <- rec_obj$steps[[3]]$sds["n"]
# c("center" = center_history, "scale" = scale_history) #print histories
# 
# # 
# # 
# # 
# # n1 <- df_processed_tbl %>% ggplot() + geom_line(aes(day, n)) +
# #   geom_line(aes(day, TAVG_F, color='red',  alpha=.7)) +
# #   geom_line(aes(day, PRCP, color='blue', alpha=.7)) +
# #   theme_tq() +
# #   labs(
# #     title = "Slice11 Counts"
# #   )
# # 
# # 
# # n1
# 
# 
# 
# 
# 
# #____lstm_____
# #model inputs: Batch splitting
# #
# lag_setting <- 7 #Prediction Window (How Far Ahead are we predicting, 
# #thats a week) is the period giving best autocorr
# batch_size <- 1  #Batch size 
# 
# train_length <- nrow(df_trn) #Training Window
# tsteps <- 7 #number of training steps, we have 7 days, so we use 7 day lags
# epochs <- 300 #need to adjust this number later based on bias/variance tradeoff
# #ie, earlystopping to prevent overfitting. 
# 
# #LSTMPLAN
# #NEED THIS FOR TOTALMODEL
# #lag_setting = nrow(df_tst)
# #batch_size = 24
# #train_length = nrow(df_train)
# #tsteps = 7
# #epochs = 300
# 
# 
# 
# #_____Tensorize inputs 
# # Training Set
# lag_train_tbl <- df_processed_tbl %>%
#   mutate(value_lag_1 = lag(n, n = lag_setting-6),
#          value_lag_2 = lag(n, n = lag_setting-5), p_lag_1 = lag(PRCP, n = lag_setting-6), t_lag_1 = lag(TAVG_F, n=lag_setting-6), 
#          value_lag_3 = lag(n, n = lag_setting-4), p_lag_2 = lag(PRCP, n = lag_setting-5), t_lag_2 = lag(TAVG_F, n=lag_setting-5), 
#          value_lag_4 = lag(n, n = lag_setting-3), p_lag_3 = lag(PRCP, n = lag_setting-4), t_lag_3 = lag(TAVG_F, n=lag_setting-4), 
#          value_lag_5 = lag(n, n = lag_setting-2), p_lag_4 = lag(PRCP, n = lag_setting-3), t_lag_4 = lag(TAVG_F, n=lag_setting-3), 
#          value_lag_6 = lag(n, n = lag_setting-1), p_lag_5 = lag(PRCP, n = lag_setting-2), t_lag_5 = lag(TAVG_F, n=lag_setting-2), 
#          value_lag_7 = lag(n, n = lag_setting-0), p_lag_6 = lag(PRCP, n = lag_setting-1), t_lag_6 = lag(TAVG_F, n=lag_setting-1)) %>%
#   filter(!is.na(value_lag_7)) %>% #drop the lookback period
#   filter(key == "training") %>%
#   tail(train_length)
# lag_train_tbl 
# 
# #Tensor shape is (num obs, num timesteps, num predictors)
# #in our case we have 17 observations, 7 timesteps, and 3 variables
# #tensor looks like (observation number, day of timestep, layer(nshift, percipshift, tempshift))
# #let's delay the temp and prcp only 1 day so we copy for multiple days
# 
# x_train <- lag_train_tbl %>% select(!c(day,key,n))
# x_train_vec <- c(x_train$value_lag_1, x_train$value_lag_2, x_train$value_lag_3, x_train$value_lag_4, x_train$value_lag_5, x_train$value_lag_6, x_train$value_lag_7, 
#                  x_train$PRCP, x_train$p_lag_1, x_train$p_lag_2, x_train$p_lag_3, x_train$p_lag_4, x_train$p_lag_5, x_train$p_lag_6, 
#                  x_train$TAVG_F, x_train$t_lag_1, x_train$t_lag_2, x_train$t_lag_3, x_train$t_lag_4, x_train$t_lag_5, x_train$t_lag_6) 
# 
# x_train_arr <- array(data = x_train_vec, dim = c(17, tsteps, 3))
# y_train_vec <- lag_train_tbl$n
# y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
# 
# 
# # Testing Set
# lag_test_tbl <- df_processed_tbl %>%
# mutate(value_lag_1 = lag(n, n = lag_setting-6),
#        value_lag_2 = lag(n, n = lag_setting-5), p_lag_1 = lag(PRCP, n = lag_setting-6), t_lag_1 = lag(TAVG_F, n=lag_setting-6), 
#        value_lag_3 = lag(n, n = lag_setting-4), p_lag_2 = lag(PRCP, n = lag_setting-5), t_lag_2 = lag(TAVG_F, n=lag_setting-5), 
#        value_lag_4 = lag(n, n = lag_setting-3), p_lag_3 = lag(PRCP, n = lag_setting-4), t_lag_3 = lag(TAVG_F, n=lag_setting-4), 
#        value_lag_5 = lag(n, n = lag_setting-2), p_lag_4 = lag(PRCP, n = lag_setting-3), t_lag_4 = lag(TAVG_F, n=lag_setting-3), 
#        value_lag_6 = lag(n, n = lag_setting-1), p_lag_5 = lag(PRCP, n = lag_setting-2), t_lag_5 = lag(TAVG_F, n=lag_setting-2), 
#        value_lag_7 = lag(n, n = lag_setting-0), p_lag_6 = lag(PRCP, n = lag_setting-1), t_lag_6 = lag(TAVG_F, n=lag_setting-1)) %>%
#   filter(!is.na(value_lag_7)) %>% #drop the lookback period
#   filter(key == "testing")
# 
# xtest <- lag_test_tbl %>% select(!c(day,key,n))
# x_test_vec <- c(xtest$value_lag_1, xtest$value_lag_2, xtest$value_lag_3, xtest$value_lag_4, xtest$value_lag_5, xtest$value_lag_6, xtest$value_lag_7, 
#                 xtest$PRCP, xtest$p_lag_1, xtest$p_lag_2, xtest$p_lag_3, xtest$p_lag_4, xtest$p_lag_5, xtest$p_lag_6, 
#                 xtest$TAVG_F, xtest$t_lag_1, xtest$t_lag_2, xtest$t_lag_3, xtest$t_lag_4, xtest$t_lag_5, xtest$t_lag_6) 
# 
# x_test_arr <- array(data = x_test_vec, dim = c(7, tsteps, 3))
# y_test_vec <- lag_test_tbl$n
# y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
# 
# 
# #___________LSTM Model 
# model <- keras_model_sequential()
# model %>%
#   layer_lstm(units            = 50, 
#              input_shape      = c(tsteps, 3), 
#              batch_size       = batch_size,
#              return_sequences = TRUE, 
#              stateful         = TRUE) %>% 
#   layer_lstm(units            = 50, 
#              return_sequences = FALSE, 
#              stateful         = TRUE) %>% 
#   layer_dense(units = 1)
# model %>% 
#   compile(loss = 'mae', optimizer = optimizer_rmsprop())
# #we use mean absolute error as our loss function since it is most robust to outliers
# #there are lots of outliers in our data. 
# 
# model
# 
# 
# for (i in 1:300) {
#   hist <- model %>% fit(x          = x_train_arr, 
#                         y          = y_train_arr, 
#                         batch_size = batch_size,
#                         epochs     = 1, 
#                         verbose    = 1,
#                         shuffle    = FALSE)
#   model %>% reset_states()
#   cat("Epoch: ", i)
#   
# }
# 
# 
# #Predicting Using the 12-day LSTM Model
# #x_test_arr, is our entire test-set tensor(with all the observations)
# #using scale_history and center_history, we retransform and square the output
# #to get the the final true count value 
# 
# # Make Predictions
# pred_out <- model %>% 
#   predict(x_test_arr, batch_size = batch_size) %>%
#   .[,1] 
# # Retransform values
# pred_tbl <- tibble(
#   index   = lag_test_tbl$day,
#   value   = (pred_out * scale_history + center_history)^2
# ) 
# # Combine actual data with predictions
# tbl_1 <- df_trn %>% select(!c(PRCP,TAVG_F)) %>% 
#   add_column(key = "actual")
# tbl_1 <-tbl_1 %>% rename(index=day,value=n)
# tbl_2 <- df_tst %>% select(!c(PRCP,TAVG_F)) %>% 
#   add_column(key = "actual")
# tbl_2 <- tbl_2 %>% rename(index=day,value=n)
# tbl_3 <- pred_tbl %>%
#   add_column(key = "predict")
# # Create time_bind_rows() to solve dplyr issue
# time_bind_rows <- function(data_1, data_2, index) {
#   index_expr <- enquo(index)
#   bind_rows(data_1, data_2) %>%
#     as_tbl_time(index = !! index_expr)
# }
# ret <- list(tbl_1, tbl_2, tbl_3) %>%
#   reduce(time_bind_rows, index = index) %>%
#   arrange(key, index) %>%
#   mutate(key = as_factor(key))
# 
# ret




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
  
  safe_rmse(prediction_tbl)$`.estimate`
  
}


#gives us the rmse of single sample test. 
#however, note that this doesnt really show us anything. 


#Make Prediction function
predict_keras_lstm <- function(split, epochs = 300, ...) {
  
  
  
  #Data Setup
  df_trn <- training(split)
  df_tst <- testing(split)
  
  df <- bind_rows( 
    df_trn %>% add_column(key="training"), 
    df_tst %>% add_columns(key='testing') 
  ) %>%
    as_tbl_time(index=day)
  
  
  #Preprocessing
  rec_obj <- recipe(n ~ ., df) %>% 
    step_sqrt(n) %>% 
    step_center(n) %>% 
    step_scale(n) %>%
    step_sqrt(PRCP) %>% 
    step_center(PRCP) %>% 
    step_scale(PRCP) %>%
    step_sqrt(TAVG_F) %>% 
    step_center(TAVG_F) %>% 
    step_scale(TAVG_F) %>%
    prep()
  
  df_processed_tbl <- bake(rec_obj, df) #counts are transformed
  df_processed_tbl
  
  center_history <- rec_obj$steps[[2]]$means["n"]
  scale_history  <- rec_obj$steps[[3]]$sds["n"]
  
  #____lstm_Plan____
  #model inputs: Batch splitting
  #
  lag_setting <- 2 
  batch_size <- 1  # we have train length is 22
  train_length <- nrow(df_trn) 
  tsteps <- 2
  epochs <- epochs 
  
  
  #_____Tensorize Train Test 
  # Training Set
  lag_train_tbl <- df_processed_tbl %>%
    mutate(value_lag_1 = lag(n, n = lag_setting-1),
           value_lag_2 = lag(n, n = lag_setting), p_lag_1 = lag(PRCP, n = lag_setting-1), t_lag_1 = lag(TAVG_F, n=lag_setting-1), 
           p_lag_2 = lag(PRCP, n = lag_setting), t_lag_2 = lag(TAVG_F, n=lag_setting))  %>%
    filter(!is.na(value_lag_2)) %>% #drop the lookback period
    filter(key == "training") %>%
    tail(train_length)
  lag_train_tbl 
  
  x_train <- lag_train_tbl %>% select(!c(day,key,n, PRCP, TAVG_F))
  x_train_vec <- c(x_train$value_lag_1, x_train$value_lag_2, 
                   x_train$p_lag_1, x_train$p_lag_2, 
                   x_train$t_lag_1, x_train$t_lag_2) 
  
  x_train_arr <- array(data = x_train_vec, dim = c(22, tsteps, 3))
  y_train_vec <- lag_train_tbl$n
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
  
  
  # Testing Set
  lag_test_tbl <- df_processed_tbl %>%
    mutate(value_lag_1 = lag(n, n = lag_setting-1),
           value_lag_2 = lag(n, n = lag_setting), p_lag_1 = lag(PRCP, n = lag_setting-1), t_lag_1 = lag(TAVG_F, n=lag_setting-1), 
           p_lag_2 = lag(PRCP, n = lag_setting), t_lag_2 = lag(TAVG_F, n=lag_setting))  %>%
    filter(!is.na(value_lag_2)) %>% #drop the lookback period
    filter(key == "testing")
  
  xtest <- lag_test_tbl %>% select(!c(day,key,n, PRCP, TAVG_F))
  x_test_vec <- c(xtest$value_lag_1, xtest$value_lag_2, 
                  xtest$p_lag_1, xtest$p_lag_2, 
                  xtest$t_lag_1, xtest$t_lag_2) 
  
  x_test_arr <- array(data = x_test_vec, dim = c(7, tsteps, 3))
  y_test_vec <- lag_test_tbl$n
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
  
  
  #___________LSTM Model Creation  
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units            = 50, 
               input_shape      = c(tsteps, 3), 
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
  
  #___________LSTM Model Fitting
  for (i in 1:epochs) {
    hist <- model %>% fit(x          = x_train_arr, 
                          y          = y_train_arr, 
                          batch_size = batch_size,
                          epochs     = 1, 
                          verbose    = 1,
                          shuffle    = FALSE)
    model %>% reset_states()
    cat("Epoch: ", i)
    
  }
  
  
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
  tbl_1 <- df_trn %>% select(!c(PRCP,TAVG_F)) %>% 
    add_column(key = "actual")
  tbl_1 <-tbl_1 %>% rename(index=day,value=n)
  tbl_2 <- df_tst %>% select(!c(PRCP,TAVG_F)) %>% 
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
  
  return(ret)
}




#PREDICTING OVER EVERYTHING
#Use prediction function over all rolling origin slices
sample_predictions_lstm_tbl <- rolling_origin_resamples %>%
  mutate(predict = map(splits, predict_keras_lstm, epochs = 300))

sample_predictions_lstm_tbl

#RMSE over All Slices
sample_rmse_tbl <- sample_predictions_lstm_tbl %>%
  mutate(rmse = map_dbl(predict, calc_rmse)) %>%
  select(id, rmse)
sample_rmse_tbl


sample_rmse_tbl %>%
  ggplot(aes(rmse)) +
  geom_histogram(aes(y = ..density..), fill = palette_light()[[1]], bins = 16) +
  geom_density(fill = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  ggtitle("Histogram of RMSE")
#no real distribution, error is kinda randomly distributed

sample_rmse_tbl %>%
  summarize(
    mean_rmse = mean(rmse),
    sd_rmse   = sd(rmse)
  )
#summary of Error 

plot_prediction <- function(data, id_s, alpha = 1, size = 2, base_size = 14) {
  rmse_val <- sample_rmse_tbl %>% filter(id == id_s) 
  rmse_valr <- rmse_val$rmse
  g <- data %>%
    ggplot(aes(index, value, color = key)) +
    geom_line(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id_s}, RMSE: {round(rmse_valr, digits = 1)}"),
      x = "", y = "")
  
  return(g)
  
}


plot_predictions <- function(sampling_tbl, predictions_col, 
                             ncol = 3, alpha = 1, size = 2, base_size = 14,
                             title = "Backtested Predictions") {
  
  predictions_col_expr <- enquo(predictions_col)
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map2(!! predictions_col_expr, id, 
                           .f        = plot_prediction, 
                           alpha     = alpha, 
                           size      = size, 
                           base_size = base_size)) 
  
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

sample_predictions_lstm_tbl %>%
  plot_predictions(predictions_col = predict, alpha = 0.7, size = 1, base_size = 10,
                   title = "Keras Stateful LSTM: Backtested Predictions")
