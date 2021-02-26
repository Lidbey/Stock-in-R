library('ggplot2')
library('forecast')
library('quantmod')
library('xts')
library('tseries')
library('timeSeries')

data <- getSymbols(Symbols="PKN.PR",src="yahoo",from=Sys.Date()-5000,to=Sys.Date(),auto.assign=FALSE)
data <- Cl(data)
data <- na.omit(data)

chart_Series(data, col = "black")
add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)

data_log<-log(data)

acf_log<-acf(data_log,lag.max=200)
pacf_log<-pacf(data_log,lag.max=200)

par(mfrow=c(2,1))
plot(acf_log)
plot(pacf_log)

par(mfrow=c(1,1))
pacf_log$lag[(pacf_log$acf)>=0.08]

data_diff<-diff(data_log,lag=1)
data_diff<-na.locf(data_diff,na.rm=TRUE,fromLast=TRUE)

plot(data_diff)
library('tseries')
adf.test(data_log) #NULL is non-stationary
adf.test(data_diff)

par(mfrow=c(2,1))
acf(data_diff)
pacf(data_diff)
par(mfrow=c(1,1))


train_to=565
predict_upto=30



train_data<-data_diff[1:train_to]

set.seed(253)
arima_model<-auto.arima(train_data,stationary=TRUE)
summary(arima_model)
checkresiduals(arima_model)

arima <- arima(train_data,order=c(2,0,2))
summary(arima)
forecast1<-forecast(arima,h=100)
plot(forecast1)
checkresiduals(arima)


arima <- arima(data_log[1:train_to],order=c(2,1,2))
summary(arima)

forecast2 <- forecast(arima,h=predict_upto)
a <- ts(data_log[1:(train_to+predict_upto)])
forecast2 %>% autoplot() + autolayer(a)

forecast3 <- forecast(arima,h=predict_upto)
forecast3$mean<-exp(forecast3$mean)
a <- ts(data[1:(train_to+predict_upto)])
forecast3 %>% autoplot() +xlim(c(train_to-1,train_to+predict_upto)) + ylim(c(250,300)) + autolayer(a)


diffvector<-c()
for (i in 1:100)
{
  train_to=564+i
  train_upto=1
  
  arima <- arima(data_log[1:train_to],order=c(2,1,2))
  forecastX <- forecast(arima,h=train_upto)
  diffvector <- c(diffvector,exp(forecastX$mean))
}

real<-data$PKN.PR.Close[566:665]
forecasting<-data$PKN.PR.Close[566:665]
forecasting$PKN.PR.Close<-diffvector
colnames(forecasting)<-"Forecast"
plot(merge(real,forecasting,real-forecasting+300),pch=19)
plot.xts(merge(real,forecasting),pch=19,type="l")
addLegend(legend.loc="bottomright",col=c("black","red"),pch=19)
realData<-data$PKN.PR.Close[565:665]
realData<-unclass(realData)
forecastingData<-unclass(forecasting)

#if we consider day '0' as current day,
#forecasting[1] is forecasting for tomorrow
#realData[1] is current day
#realData[2] is tomorrow real day price
dRevenue=c();
sum=0;
buys=0;
amount=0;
success=0;
failure=0;
for (i in 1:100)
{
  if (forecasting[i]>realData[i])
  {
    dRevenue=c(dRevenue,(realData[i+1]*0.9961)-(realData[i]*1.0039))
    sum=sum+(realData[i+1]*0.9961)-(realData[i]*1.0039); #consider 0.39% fee
    buys=buys+1;
    amount=amount+realData[i]
    if(realData[i+1]>realData[i])
      success=success+1;
    if(realData[i+1]<realData[i])
      failure=failure+1;
  }
}
c(sum,buys,amount)
sum/((amount)/buys)*100
c(success,failure)


#conclusion is not very good ;)



