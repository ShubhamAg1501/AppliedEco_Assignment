wpitimeseries <- ts(wpi,frequency=12)
plot.ts(wpitimeseries)
wpitimeseriesdiff1 <- diff(wpitimeseries, differences=1)
plot.ts(wpitimeseriesdiff1) 

library(tseries)
adf.test((wpitimeseries), alternative="stationary", k=0)
adf.test((wpitimeseriesdiff1), alternative="stationary", k=0)
library(aTSA)
stationary.test(wpitimeseries) 
stationary.test(wpitimeseries, method = "pp") 
stationary.test(wpitimeseries, method = "kpss") 

stationary.test(wpitimeseriesdiff1) 
stationary.test(wpitimeseriesdiff1, method = "pp") 
stationary.test(wpitimeseriesdiff1, method = "kpss") 

acf(wpitimeseriesdiff1, lag.max=20) 
acf(wpitimeseriesdiff1, lag.max=20, plot=FALSE) 
pacf(wpitimeseriesdiff1, lag.max=20) 
pacf(wpitimeseriesdiff1, lag.max=20, plot=FALSE) 

library(forecast)
library(urca)
auto.arima(wpi)                 
auto.arima(wpitimeseries)      
auto.arima(wpitimeseriesdiff1)   

wpitimeseriesarima <- arima(wpitimeseries, order=c(2,1,0))
wpitimeseriesarima 

wpitimeseriesforecasts <- forecast(wpitimeseriesarima, h=5)
wpitimeseriesforecasts 
plot(wpitimeseriesforecasts)

acf(wpitimeseriesforecasts$residuals, lag.max=20)
Box.test(wpitimeseriesforecasts$residuals, lag=20, type="Ljung-Box") 
plot.ts(wpitimeseriesforecasts$residuals)
hist(wpitimeseriesforecasts$residuals)