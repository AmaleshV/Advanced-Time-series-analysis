library(readxl)
mydata<-read_excel('GDP.xlsx')
mydata1<-read_excel('CPI.xlsx')
#Try linear regression 1st #
# Linear regression ####
attach(mydata)
length(GDP_ts)
GDP_ts <- ts(GDP, frequency = 4,start = c(1988,1))
TREND<-1:123
ts.plot(TREND)
Q1<-c(rep(c(1,0,0,0),30),c(1,0,0));Q1
Q2<-c(rep(c(0,1,0,0),30),c(0,1,0));Q2
Q3<-c(rep(c(0,0,1,0),30),c(0,0,1));Q3
Q4<-c(rep(c(0,0,0,1),30),c(0,0,0));Q4
fit<-lm(GDP_ts~TREND+Q1+Q2+Q3+Q4)
summary(fit)
# as per the model, none of the quarters are significantly better in GDP than Q4!
# Linear regression residual plot
ts.plot(fit$residuals,main = "Residuals plot") #lack homoscedasticity, hence model is not valid!
# Time Series Analysis####
## Univariate analysis ####
# Declare data to be time series
GDP_ts <- ts(GDP, frequency = 4,start = c(1988,1))
ts.plot(GDP_ts)
#Plot of GDP
logGDP_ts<- log(GDP_ts)
par(mfrow =c(1,1))
ts.plot(log(GDP_ts),ylab="GDP", main="log transformed GDP") # Descriptive statistics
summary(logGDP_ts)
# Autocorrelation
acf(logGDP_ts)
#Augmented DF test to check stationarity
library(CADFtest)
max.lag<-round(sqrt(length(logGDP_ts)));max.lag #11
CADFtest(logGDP_ts, type= "trend", criterion= "BIC", max.lag.y=max.lag)
#p 0.818
# GDP in differences
dlogGDP_ts<-diff((logGDP_ts)); dlogGDP_ts
par(mfrow =c(1,2))
ts.plot(dlogGDP_ts,ylab="Change in GDP", main="Change in GDP")
monthplot(dlogGDP_ts,ylab="Change in GDP", main="Quarterly change in GDP") #not much seasonality

#Augmented DF test
library(CADFtest)
max.lag<-round(sqrt(length(dlogGDP_ts))) #11
CADFtest(dlogGDP_ts, type= "drift", criterion= "BIC", max.lag.y=max.lag)
#The p-value -> 0:00 < 5%. We reject H0
#and conclude that the time series is stationary.
# Hence, time series are integrated of order one.
## Key correlograms ####
par(mfrow=c(1,2))
acf(dlogGDP_ts,main= "ACF of stationary series") #there are some significant correlations until lag1
pacf(dlogGDP_ts,main= "PACF of stationary series") #there are some significant correlations until lag3
Box.test(dlogGDP_ts, lag = max.lag, type = "Ljung-Box")
#There are many significant correlations and partial correlations. The time series is not white noise.

#ARMA (1,0) ####
fit_ar1<-arima(logGDP_ts,order=c(1,1,0),seasonal=c(0,0,0))
fit_ar1
AIC(fit_ar1,k=log(123))
Box.test(fit_ar1$residuals,lag=max.lag,type="Ljung-Box")
par(mfrow=c(1,1))
plot(fit_ar1$residuals) #heteroskedasticity
par(mfrow=c(1,2))
acf(fit_ar1$residuals)
pacf(fit_ar1$residuals)
#ar2
fit_ar2<-arima(logGDP_ts,order=c(2,1,0))
fit_ar2
AIC(fit_ar2,k=log(123))
#ARMA (1,1) ####
fit_arma11<-arima(logGDP_ts,order=c(1,1,1))
fit_arma11
AIC(fit_arma11,k=log(123))
Box.test(fit_arma11$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_arma11$residuals) 
acf(fit_arma11$residuals) 
pacf(fit_arma11$residuals) 
acf(fit_arma11$residuals^2)
#ARMA (1,2) ####
fit_arma12<-arima(logGDP_ts,order=c(1,1,2))
fit_arma12
AIC(fit_arma11,k=log(123))
Box.test(fit_arma11$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_arma11$residuals) 
acf(fit_arma11$residuals) 
pacf(fit_arma11$residuals) 
acf(fit_arma11$residuals^2)
fit_ma2<-arima(logGDP_ts,order=c(0,1,2))
fit_ma2
par(mfrow=c(1,1))
plot(fit_ma2$residuals) 
acf(fit_ma2$residuals) 
pacf(fit_ma2$residuals) 
acf(fit_ma2$residuals^2)
fit_ma3<-arima(logGDP_ts,order=c(0,1,3))
fit_ma3
par(mfrow=c(1,1))
plot(fit_ma3$residuals) 
acf(fit_ma3$residuals) 
pacf(fit_ma3$residuals) 
acf(fit_ma3$residuals^2)

fit_arma13<-arima(logGDP_ts,order=c(1,1,3))
fit_arma13
par(mfrow=c(1,1))
plot(fit_arma13$residuals) 
acf(fit_arma13$residuals) 
pacf(fit_arma13$residuals) 
acf(fit_arma13$residuals^2)
AIC(fit_arma13,k=log(123))

fit_sarma21<-arima(logGDP_ts,order=c(0,1,2),seasonal = list(order = c(0, 0, 1))) #middle zero for seasonality from monthplot
fit_sarma21
AIC(fit_sarma21,k=log(123)) #BIC= 1247.5 further decreased!
Box.test(fit_sarma21$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_sarma21$residuals) #heteroskedasticity
acf(fit_sarma21$residuals) #no autocorrelations!
pacf(fit_sarma21$residuals) # no problem again!
acf(fit_sarma21$residuals^2) ##correlogram of the squared et shows few significant correlations


fit_sarma21<-arima(logGDP_ts,order=c(0,1,2),seasonal = list(order = c(0, 0, 2))) #middle zero for seasonality from monthplot
fit_sarma21
AIC(fit_sarma21,k=log(123)) #BIC= 1247.5 further decreased!
Box.test(fit_sarma21$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_sarma21$residuals) #heteroskedasticity
acf(fit_sarma21$residuals) #no autocorrelations!
pacf(fit_sarma21$residuals) # no problem again!
acf(fit_sarma21$residuals^2) ##correlogram of the squared et shows few significant correlations


fit_sarma21<-arima(logGDP_ts,order=c(0,1,3),seasonal = list(order = c(0, 0, 1))) #middle zero for seasonality from monthplot
fit_sarma21
AIC(fit_sarma21,k=log(123)) #BIC= 1247.5 further decreased!
Box.test(fit_sarma21$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_sarma21$residuals) #heteroskedasticity
acf(fit_sarma21$residuals) #no autocorrelations!
pacf(fit_sarma21$residuals) # no problem again!
acf(fit_sarma21$residuals^2) ##correlogram of the squared et shows few significant correlations

fit_sarma21<-arima(logGDP_ts,order=c(0,1,3),seasonal = list(order = c(0, 0, 2))) #middle zero for seasonality from monthplot
fit_sarma21
AIC(fit_sarma21,k=log(123)) #BIC= 1247.5 further decreased!
Box.test(fit_sarma21$residuals,lag=max.lag,type="Ljung-Box") # white noise, valid model!
par(mfrow=c(1,1))
plot(fit_sarma21$residuals) #heteroskedasticity
acf(fit_sarma21$residuals) #no autocorrelations!
pacf(fit_sarma21$residuals) # no problem again!
acf(fit_sarma21$residuals^2) ##correlogram of the squared et shows f