####IMPORT DATASET end library####
AAPL = read.csv("/Users/edoar/Desktop/UNICATT/2° Anno/TIME SERIES/Santucci_Edoardo/Project/AAPL2018.csv")


library(ggplot2)
library(forecast)
library(tseries)
library(gstat)
library(tidyverse)
library(urca)
library(rugarch)
library(zoo)
library(PerformanceAnalytics)

####EDA####
AAPL$Date = as.Date(AAPL$Date)
head(AAPL)
summary(AAPL)
sum(is.na(AAPL))

ggplot(data = AAPL, aes(x = Date)) +
  geom_line(aes(y = Open, color = "Open")) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "AAPL prices progression", x = "Date", y = "Value") +
  theme_minimal()

AAPL.close = data.frame('date' = AAPL$Date, 'close' = AAPL$Close)
 
close_ts <- ts(AAPL.close$close, start = c(2018, 1), frequency = 251) 
close_ts
plot.ts(close_ts)

####ARIMA####

##check the stationarity##
dec_ts = decompose(close_ts, type = 'mult') 
plot(dec_ts)#we note the presence of seasonality and trends
adf.test(close_ts) #P-VALUE = 0.2698 NOT STATIONARY
## with the ADF(Augumented Dickey Fuller) we check if the time series is stationary.
##the null hypothesis of the ADF test is that the time series is not-stationary, 
##so if the p-value of the test is less than the significance level (0.05) than we can reject
##the null hypothesis and deduce that the time series is stationary.

##forecast not diff##
train <- head(close_ts, round(length(close_ts) -251)) #Creation of the training set using the first 251 values of close_ts
h <- length(close_ts) - length(train)+1
test <- tail(close_ts, h) #Creation of the test set using the last h values of close_ts
autoplot(train, series = "Training Set", color = "black") +
  autolayer(test, series = "Test Set", color = "red") +
  labs(title = "Training and Test Sets", x = "Time", y = "Value") +
  theme_bw()

model = auto.arima(train, ic = 'aic', trace = TRUE)# the auto.arima function tells us that the best model is ARIMA(0,1,1)
autoplot(model)
forecast(model)

pred = forecast(model, h = length(test)) #Forecast generation using the trained ARIMA model
autoplot(pred,level = c(95),series = "Predicted") +
  autolayer(test, series = "Test Data")
accuracy(pred, test)

##differencing to make it stationary##
diff_close_ts = diff(close_ts, 1)
plot(diff_close_ts)
adf.test(diff_close_ts)
acf(diff_close_ts)
pacf(diff_close_ts)
ndiffs(diff_close_ts, test = 'adf')
#now is stationary P-VALUE = 0.01

##forecast diff##
train_diff <- head(diff_close_ts, round(length(diff_close_ts) -251))
h <- length(diff_close_ts) - length(train_diff)+1
test_diff <- tail(diff_close_ts, h)
autoplot(train_diff, series = "Training Set", color = "black") +
  autolayer(test_diff, series = "Test Set", color = "red") +
  labs(title = "Training and Test Sets", x = "Time", y = "Value") +
  theme_bw()
#
model_diff = auto.arima(train_diff, ic = 'aic', trace = TRUE)
autoplot(model_diff)
forecast(model_diff)

pred_diff = forecast(model_diff, h = length(test_diff))
autoplot(pred_diff,level = c(95),series = "Predicted") +
  autolayer(test_diff, series = "Test Data")

accuracy(pred_diff, test_diff)

##forecast log diff##
log_close_ts = diff(log(close_ts), 1)
plot(log_close_ts)
adf.test(log_close_ts)
acf(log_close_ts)
pacf(log_close_ts)
ndiffs(log_close_ts, test = 'adf')


train_log <- head(log_close_ts, round(length(log_close_ts) -251))
h <- length(log_close_ts) - length(train_log)+1
test_log <- tail(log_close_ts, h)
autoplot(train_log, series = "Training Set", color = "black") +
  autolayer(test_log, series = "Test Set", color = "red") +
  labs(title = "Training and Test Sets", x = "Time", y = "Value") +
  theme_bw()
#
model_log = auto.arima(train_log, ic = 'aic', trace = TRUE)
autoplot(model_log)
forecast(model_log)

pred_log = forecast(model_log, h = length(test_log))
autoplot(pred_log,level = c(95),series = "Predicted") +
  autolayer(test_log, series = "Test Data")

accuracy(pred_log, test_log)

summary(model)
summary(model_diff)
summary(model_log)
checkresiduals(model)
checkresiduals(model_diff)
checkresiduals(model_log)
# residuals are good but not optimal: the histogram shows that the residual distribution
# is not completely Normal but we denote a certain similarity with a more leptokurtic
# distribution like a t-student or even a Cauchy distribution
# Regarding the ACF we have optimal residuals while for the residuals plot we notice
# that in the beginning we have less noisy residuals (due to the fact that the stock was not volatile)
# when Apple began to increase in terms of popularity and capitalization the stock became
# more volatile and with it also the residuals since the predictions are more likely to deviate from the 
# realized prices

####GARCH####

AAPL.z = zoo(x=AAPL.close$close, order.by=AAPL.close$date) #Creation of a 'zoo' object with time series data to handle ordered time series
Return = Return.calculate(AAPL.z, method = "log")[-1] #We use the logarithmic method to calculate the returns.
head(Return)

##check the normality##
hist(Return)
chart.Histogram(Return, methods = c('add.density', 'add.normal'), colorset = c('grey', 'darkblue', 'red'))
#from this chart the first thing that we note is that the curve of returns is a bit taller
#also we can see that the tail of returns'curve is are higher in same point


##check the stationarity##

adf_return = ur.df(Return, type = "drift",selectlags = "AIC")
summary(adf_return)
#We can reject $H_0$ (the time series is not stationary), therefore the time series is stationary.

##check the volatility##
# plot returns with squared and absolute returns
return.plot = cbind(Return, Return^2)
colnames(return.plot) = c("Returns", "Returns^2")
plot.zoo(return.plot, main="AAPL Daily Returns, Square Returns",cex.lab=1.5, cex.axis=1.5,cex.main=2.3, col="blue")

##annualized volatility##
sd(Return)
chart.RollingPerformance(R = Return,
                         width = 30,
                         FUN = 'sd.annualized',
                         main = "APPL's monthly rolling volatility")
##we note that volatility becomes high as soon as the outbreak of the pandemic in 2020
##GARCH MODEL##
G_model = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(0,0)),
                     distribution.model="norm")
G_fit = ugarchfit(data = Return, spec = G_model)
G_fit
plot(G_fit, which = 'all')

##forecast with garch##
G_forecast = ugarchforecast(fitORspec = G_fit,
                            n.ahead = 30)
plot(G_forecast)
plot(fitted(G_forecast))
plot(sigma(G_forecast))

