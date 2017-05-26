setwd('C:/Users/m.galvani/Desktop/RIDS/Terrorism/Script')
rm(list=ls())

source("./function.R")
source("forecast.R")  

library(forecast)

load('./../dati/data_cluster_europe.RData')

data <- data[data$region_txt=='Europe - Western Europe',]
#data <- data[data$cluster==4,]

#######################################
########## TIME PREDICTION ############
#######################################

time_series <- data.frame(table(data$timestamp))
names(time_series) <- c('timestamp','nattack')
time_series$timestamp <- as.POSIXct(time_series$timestamp)

all_day <- data.frame(seq(min(time_series$timestamp), max(time_series$timestamp), by="days"))
names(all_day) <- 'day'
all_day$day <- as.POSIXct(round(all_day$day, units = "days"))

time_series <- merge(all_day,time_series,by.y='timestamp',by.x='day',all=T)
time_series$nattack[is.na(time_series$nattack)] <- 0

train <- time_series[time_series$day>=as.POSIXct('2002-01-01', format='%Y-%m-%d') & time_series$day<as.POSIXct('2015-01-01', format='%Y-%m-%d'),]
test <- time_series[time_series$day>=as.POSIXct('2002-01-01', format='%Y-%m-%d') & time_series$day>=as.POSIXct('2015-01-01', format='%Y-%m-%d'),]

####################################
####
years <- format(train$day, "%Y")
tab <- table(years)
## number of days per year after removing 2014
mean(tab[1:(length(tab) - 1)])

ts <- ts(train$nattack, start = 2002, end=2015, frequency =365)
ts_tot <- ts(time_series$nattack, start = 2002, end=2016, frequency =365)

# plot series
plot(ts)

### stl decomposition
fit.stl <- stl(ts, s.window = 365)
plot(fit.stl)
sts <- fit.stl$time.series
trend <- sts[, "trend"]
fore <- forecast(fit.stl, h = 365, level = 95)
plot(fore)
accuracy(fore,test$nattack)

plot(ts_tot,  main="Forecasts for number of terrorist attacks")
lines(round(fore$mean), col='red')

result.stl <- forecastStl(time_series, n.ahead = 365)
plotForecastResult(result.stl, title = "Exchange rate forecasting with STL")
# 
result <- subset(result.stl, date >= "2013-01-01")
plotForecastResult(result, title = "Exchange rate forecasting with STL (2014)")

######## EXPONETIAL MODEL
# fit <- HoltWinters(ts, beta=FALSE, gamma=FALSE)
# # double exponential - models level and trend
# fit <- HoltWinters(myts, gamma=FALSE)
# 
# fore <- forecast(fit, 90)
# plot(forecast(fit, 3))
# 
# accuracy(fore)
# 
# ############ ARIMA
# result.arima <- forecastArima(time_series, n.ahead = 90)
# 
# ############
# fit <- ets(ts)

# Automated forecasting using an ARIMA model
fit <- auto.arima(ts)
plot(forecast(fit,h=365))


