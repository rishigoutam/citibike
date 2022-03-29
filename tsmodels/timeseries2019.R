library(tidyverse)
library(ggcorrplot)
library(gtools)
library(lubridate)
library(fpp3)
library(feasts)
library(patchwork)
library(slider)
library(seasonal)
library(forecast)
library(astsa)
#library(tsdl)


data <- read_csv('../data/NY_2019.csv')
data <- data[-1]
#we try summarize_at hour(data$starttime)
data$min <- round_date(data$starttime, unit="minute")
data$hour <- round_date(data$starttime, unit="hour")
data$day <- date(data$starttime)
data$week <- round_date(data$starttime, unit="week")
data$month <- yearmonth(data$starttime)
data$quarter<- yearquarter(data$starttime)


#the min count (Too dense to plot, crashes R )
data %>% count(min)

#the Hourly Count 
data %>% count(hour) 
data %>% count(hour) %>% as_tsibble() %>% autoplot()

#the day count 
data %>% count(day)
data %>% count(day) %>% as_tsibble() %>% autoplot()

#the Weekly count 
data %>% count(week)
data %>% count(week) %>% ungroup() %>% select(n) %>%  ts() %>% autoplot()

#the Monthly Count
data %>% count(month)
data %>% count(month) %>% as_tsibble() %>% autoplot()


#The quarterly Count 
data %>% count(quarter)
data %>% count(quarter) %>% as_tsibble() %>% autoplot()

#Hourly Values keep the Noise in, and the other values give us an indication
# That we have clear trend and seasonality 

#Time-Series Analysis______________ Single Differenceing Looks good
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% acf2(main='Hourly ACF, PCF')
#so we can see there is strong periodicity so we have to do some differencing
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% acf2(main='1D, Hourly ACF, PCF')
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% diff(1) %>% acf2(main='2D, Hourly ACF, PCF')

#Ljung-Box Test we want p< .05
d <- data %>% count(hour) %>% ungroup() %>% select(n) %>% ts()
d.d <-data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1)
d %>% Box.test(type="Ljung-Box", lag = log(length(d)))
d.d %>% Box.test(type="Ljung-Box", lag = log(length(d.d)))

#_________________________________________________________________________________SARIMA 
#_seasonal differencing 
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(30) %>% plot.ts(main="Seasonal 24")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(12) %>% plot.ts(main="Seasonal 12")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(6) %>% plot.ts(main="Seasonal 6")

data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(23) %>% acf2(main="1Diff 23  ACF/PACF Seasonal")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(23) %>% acf2(main="1Diff 24  ACF/PACF Seasonal")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(12) %>% acf2(main="1Diff 12 ACF/PACF Seasonal")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(6) %>% acf2(main="1Diff 6 ACF/PACF Seasonal")


data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(23) %>% diff(23) %>% 
  acf2(main="2Diff 23 mo ACF/PACF Seasonal")
data %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(24) %>% diff(24) %>% 
  acf2(main="2Diff 24 mo ACF/PACF Seasonal")


#d = 1 , normal differencing
#qmax = 4 , ACF gives us qmax
#pmax = 5 , PACF gives us pmax

#2ce differenced, 24 month seasons, with q_max_seas= 13, p_max_seas = 4

#_______________________________________SARIMA Full

set <- data %>% count(hour) %>% ungroup() %>% select(n) %>% ts()
d= 1
DD= 2
p_max= 2
q_max= 7 
p_s_max=2
q_s_max=2
per= 24
for(p in 1:p_max){
  for(q in 1:q_max){
    for(p_seasonal in 1:p_s_max){
      for(q_seasonal in 1:q_s_max){
        if(p+d+q+p_seasonal+DD+q_seasonal<=(p_max+q_max+p_s_max+q_s_max+d+DD)){
          model<-arima(x=set, order = c((p-1),d,(q-1)), seasonal = list(order=c((p_seasonal-1),DD,(q_seasonal-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,p_seasonal-1,DD,q_seasonal-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#p-vals all zero, start from scratch with new dataset





















#______________________________________________________________________Daily 

#Daily Values keep the Noise in, but wayyy less noisy than hourly

#Time-Series Analysis______________ Single Differenceing Looks good,  p looks like 2, q looks like 1 
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% acf2(main='Daily ACF, PCF')
#so we can see there is strong periodicity so we have to do some differencing
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% acf2(main='1D, Daily ACF, PCF')
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% diff(1) %>% acf2(main='2D, Daily ACF, PCF')
#double diff is too much residue


#Ljung-Box Test we want p< .05
d <- data %>% count(day) %>% ungroup() %>% select(n) %>% ts()
d.d <-data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(1)
d %>% Box.test(type="Ljung-Box", lag = log(length(d))) # p = < 2.2e-16
d.d %>% Box.test(type="Ljung-Box", lag = log(length(d.d))) # p = 6.14e-11 <- single differencing 

#_________________________________________________________________________________SARIMA 
#_seasonal differencing 
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(7) %>% plot.ts(main="Seasonal 7")  #most even, weekly
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(2) %>% plot.ts(main="Seasonal 12")
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(64) %>% plot.ts(main="Seasonal 6") 

data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(7) %>% acf2(main="1Diff 7  ACF/PACF Seasonal")
data %>% count(day) %>% ungroup() %>% select(n) %>% ts() %>% diff(7) %>% diff(7) %>% acf2(main="2Diff 7  ACF/PACF Seasonal")


#d = 1 , normal differencing
#qmax = 2 , ACF gives us qmax
#pmax = 2 , PACF gives us pmax

#1ce differenced, 4 seasons, with q_max_seas= 1, p_max_seas = 1

#_______________________________________SARIMA Full

set <- data %>% count(day) %>% ungroup() %>% select(n) %>% ts()
d= 1
DD= 1
p_max= 2
q_max=3 
p_s_max=2
q_s_max=2
per= 7
for(p in 1:p_max){
  for(q in 1:q_max){
    for(p_seasonal in 1:p_s_max){
      for(q_seasonal in 1:q_s_max){
        if(p+d+q+p_seasonal+DD+q_seasonal<=(p_max+q_max+p_s_max+q_s_max+d+DD)){
          model<-arima(x=set, order = c((p-1),d,(q-1)), seasonal = list(order=c((p_seasonal-1),DD,(q_seasonal-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,p_seasonal-1,DD,q_seasonal-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#SARIMA(1 1 1 0 1 1 7) AIC= 7761.436  SSE= 53381920877  p-VALUE= 0.821252 
sarima = arima(x=set, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = per))
predict = forecast(sarima, h=14, level = 85)
autoplot(predict) +ylab("Rider Count") + labs(title ="Two-week SARIMA(1,1,1,0,1,1,7) Prediction for NYC Bike Demand") +
  xlab("Days from Jan 1 2019")

#SARIMA(1 1 2 1 1 1 7) AIC= 7764.888  SSE= 53258078603  p-VALUE= 0.8779478 
sarima = arima(x=set, order = c(1,1,2), seasonal = list(order = c(1,1,1), period = per))
predict = forecast(sarima, h=14, level = 85)
autoplot(predict) +ylab("Rider Count") + labs(title ="Two-week SARIMA(1,1,2,1,1,1,7) Prediction for NYC Bike Demand") +


















#______________________________________________________________________Hourly, but with first week (Trying diff weeks 
# gives us different workable sarimas, but two weeks together is too much noise)
months <- data %>% filter(week(week) %in% c(2))

#Time-Series Analysis______________
months %>% count(hour) %>% as_tsibble() %>% autoplot()
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% plot.ts()
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% acf2(main='Hourly ACF, PCF, Months')
#so we can see there is strong periodicity so we have to do some differencing
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% acf2(main='1D, Hourly ACF, PCF')
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% diff(1) %>% acf2(main='2D, Hourly ACF, PCF')

#Ljung-Box Test we want p< .05
d <- months %>% count(hour) %>% ungroup() %>% select(n) %>% ts()
d.d <-months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1)
d.dd <-months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(1) %>% diff(1)
d %>% Box.test(type="Ljung-Box", lag = log(length(d)))
d.d %>% Box.test(type="Ljung-Box", lag = log(length(d.d)))
d.dd %>% Box.test(type="Ljung-Box", lag = log(length(d.dd)))


#Stick with single differencing

#_________________________________________________________________________________SARIMA 
#_seasonal differencing 
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(24) %>% plot.ts(main="Seasonal 24")
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(12) %>% plot.ts(main="Seasonal 12")
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(6) %>% plot.ts(main="Seasonal 6")

months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(24) %>% acf2(main="1Diff 23  ACF/PACF Seasonal")


months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(24) %>% 
  acf2(main="1Diff 24 hr ACF/PACF Seasonal")
months %>% count(hour) %>% ungroup() %>% select(n) %>% ts() %>% diff(24) %>% diff(24) %>% 
  acf2(main="2Diff 24 hr ACF/PACF Seasonal")


#d = 1 , normal differencing
#qmax = 2 , ACF gives us qmax
#pmax = 2 , PACF gives us pmax

#2ce differenced, 24 hr seasons, with q_max_seas= 3, p_max_seas = 3

#_______________________________________SARIMA Full
set <- months %>% count(hour) %>% ungroup() %>% select(n) %>% ts()
d= 1
DD= 2
p_max= 1
q_max=2 
p_s_max=3
q_s_max=3
per= 24
for(p in 1:p_max){
  for(q in 1:q_max){
    for(p_seasonal in 1:p_s_max){
      for(q_seasonal in 1:q_s_max){
        if(p+d+q+p_seasonal+DD+q_seasonal<=(p_max+q_max+p_s_max+q_s_max+d+DD)){
          model<-arima(x=set, order = c((p-1),d,(q-1)), seasonal = list(order=c((p_seasonal-1),DD,(q_seasonal-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,p_seasonal-1,DD,q_seasonal-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
#SARIMA( 1 1 0 2 1 24) AIC= 1888.498  SSE= 33526008  p-VALUE= 0.1447814 for week 1 
sarima = arima(x=set, order = c(1,1,0), seasonal = list(order = c(0,2,1), period = per))
predict = forecast(sarima, h=12, level = 85)
autoplot(predict) +ylab("Rider Count") + labs(title ="12-hour SARIMA(1,1,1,0,1,1,7) Prediction for NYC Bike Demand") +
  xlab("Hours from Jan 1 2019")


#SARIMA(0 1 1 2 2 2 24) AIC= 1888.536  SSE= 24349069  p-VALUE= 0.2653764 
sarima = arima(x=set, order = c(0,1,1), seasonal = list(order = c(2,2,2), period = per))
predict = forecast(sarima, h=12, level = 85)
autoplot(predict) +ylab("Rider Count") + labs(title ="One-Day SARIMA(1,1,1,0,1,1,7) Prediction for NYC Bike Demand") +
  xlab("Hours from Jan 1 2019")

