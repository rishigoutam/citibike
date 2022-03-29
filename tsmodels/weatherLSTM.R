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

#Join
colnames(weather)[1] <- "day"
daily <- full_join(daily,weather)

daily

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


rolling_origin_resamples$splits[[2]] %>%
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
df



#preprocessing--LSTM wants centered and normalized and scaled
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


#We record centering and scaling history in order to inver the 
#centering and scaling transformations after the model
center_history <- rec_obj$steps[[2]]$means["n"]
scale_history  <- rec_obj$steps[[3]]$sds["n"]
c("center" = center_history, "scale" = scale_history) #print histories




n1 <- df_processed_tbl %>% ggplot() + geom_line(aes(day, n)) +
  geom_line(aes(day, TAVG_F, color='red',  alpha=.7)) +
  geom_line(aes(day, PRCP, color='blue', alpha=.7)) +
  theme_tq() +
  labs(
    title = "Slice11 Counts"
  )


n1





#____lstm_____
#model inputs: Batch splitting
#
lag_setting <- 7 #Prediction Window (How Far Ahead are we predicting, 
#thats a week)
batch_size <- 24  #Batch size is the period giving best autocorr
#since 168 doesn't divide 24*24 (train window) evenly,
#we choose gcf(24*24,24*7) and thats 24. 

train_length <- nrow(df_trn) #Training Window
tsteps <- 7 #number of training steps, we have 7 days, so we use 7 day lags
epochs <- 300 #need to adjust this number later based on bias/variance tradeoff
#ie, earlystopping to prevent overfitting. 

#LSTMPLAN
#NEED THIS FOR TOTALMODEL
#lag_setting = nrow(df_tst)
#batch_size = 24
#train_length = nrow(df_train)
#tsteps = 7
#epochs = 300



#_____Tensorize inputs 
# Training Set
lag_train_tbl <- df_processed_tbl %>%
  mutate(value_lag_1 = lag(n, n = lag_setting-6),
         value_lag_2 = lag(n, n = lag_setting-5), p_lag_1 = lag(PRCP, n = lag_setting-6), t_lag_1 = lag(TAVG_F, n=lag_setting-6), 
         value_lag_3 = lag(n, n = lag_setting-4), p_lag_2 = lag(PRCP, n = lag_setting-5), t_lag_2 = lag(TAVG_F, n=lag_setting-5), 
         value_lag_4 = lag(n, n = lag_setting-3), p_lag_3 = lag(PRCP, n = lag_setting-4), t_lag_3 = lag(TAVG_F, n=lag_setting-4), 
         value_lag_5 = lag(n, n = lag_setting-2), p_lag_4 = lag(PRCP, n = lag_setting-3), t_lag_4 = lag(TAVG_F, n=lag_setting-3), 
         value_lag_6 = lag(n, n = lag_setting-1), p_lag_5 = lag(PRCP, n = lag_setting-2), t_lag_5 = lag(TAVG_F, n=lag_setting-2), 
         value_lag_7 = lag(n, n = lag_setting-0), p_lag_6 = lag(PRCP, n = lag_setting-1), t_lag_6 = lag(TAVG_F, n=lag_setting-1)) %>%
  filter(!is.na(value_lag_7)) %>% #drop the lookback period
  filter(key == "training") %>%
  tail(train_length)
lag_train_tbl 

#Tensor shape is (num obs, num timesteps, num predictors)
#in our case we have 17 observations, 7 timesteps, and 3 variables

x_train <- lag_train_tbl %>% select(!c(day,key,n))
x_train_vec <- c(x_train$value_lag_1, x_train$value_lag_2, x_train$value_lag_3, x_train$value_lag_4, x_train$value_lag_5, x_train$value_lag_6, x_train$value_lag_7, 
                 x_train$PRCP, x_train$p_lag_1, x_train$p_lag_2, x_train$p_lag_3, x_train$p_lag_4, x_train$p_lag_5, x_train$p_lag_6, 
                 x_train$TAVG_F, x_train$t_lag_1, x_train$t_lag_2, x_train$t_lag_3, x_train$t_lag_4, x_train$t_lag_5, x_train$t_lag_6) 

x_train_arr <- array(data = x_train_vec, dim = c(17, tsteps, 3))
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



