###Manisha Bhowmik 
###ECON 467 
###Final Project 
###12/11/20 

###New York Unemployment Data 
library(forecast)
library(fpp2)
library(tidyverse)
library(seasonal)

###1a 
nyurn=read.csv("NYURN.csv")
nyurn.data=ts(nyurn$NYURN,start=c(1980,1),freq=12)
autoplot(nyurn.data) + xlab("Time") + ylab("unemployement rate") +ggtitle("New York Unemployment Rate")
ggseasonplot(nyurn.data, year.labels=TRUE)
##The time series plot shows that there is no general increasing 
##or decreasing trend in the data. However, a cyclical pattern is 
##evident in which there is a decreasing period lasting for about 
##10 years and increasing period for about 5 years. The seasonal and 
##subseries plot both indicate that there is a slight seasonal pattern 
##by which unemployment rate increases from January to Feburary, then
##decreases until April, and then rises again until July 
##after which there is no identifiable seasonal pattern. 
##Generally, there appears to be peaks in Feburary and July, 
##as well as dips in April. 2020 as expected, stands out in the
##seasonal plot with a completely different pattern in which there
##is a huge jump from March to April which as we know is a result
##of the coronavirus pandemic. 

###1b 
##The time series plot shows slightly different variation at different 
##levels of series, especially the huge jump in unemployment in 2020
##so a transformation can be useful. A boxcox transformation can be 
##performed by automatically selecting lambda as shown below. 
lambda <- BoxCox.lambda(nyurn.data)
autoplot(BoxCox(nyurn.data,lambda=lambda))
##Since a BoxCox transformation may result in large prediction
##intervals and the data involves large spikes and dips, 
##I will choose not to use BoxCox. 

###1c 
##naive method
nyurn.train=window(nyurn.data, start=1976, end=c(2015,12))
nyurn.test=window(nyurn.data,start=2016,end=c(2019,12))
autoplot(nyurn.train)
naivefit.nyurn <- rwf(nyurn.train,h=48)
autoplot(naivefit.nyurn)
checkresiduals(naivefit.nyurn)
accuracy(naivefit.nyurn,nyurn.test)
##I do not think the naive method is reasonable because 
##the residuals of the plot indicate autocorrelation and
##are not normally distributed. Since the naive method
##only takes into account seasonality and trend, this method
##is not appropiate for the New York unemployement data as 
##there is no trend. Other methods such as ARIMA might 
##be more appropiate since it takes into account autocorrelation. 

###1d 
##STL Decomposition
fit2 <- stl(nyurn.data, s.window=13)
autoplot(fit2)
##This is an additive decomposition of the data. The plot 
##reveals there is a strong seasonal component and a strong 
##cyclical component. However, the variation in the seasonal
##component appears to change throughout time. 

###1e 
###Based on the plots of seasonal components created earlier, 
###the unemployment data does not have a seasonal component 
## such that the variance has a trend of either increasing or 
##decreasing. Similarly, the errors do not follow a trend 
##of either increasing or decreasing throughout time. Therefore,
##the an ETS model that includes an additive error component 
##and an additive seasonal component would be most appropiate.
##Since, the data does not have a long term trend component,
##the ETS odel (A,N,A) might be most appropiate. However, 
##note that there is an option to automatically select an
##ETS model using the ets() function. In this case, the model
##ETS(A,Ad,A) is selected therefore I will proceed as such. 

ets(nyurn.train)
ets.fit1 <- ets(nyurn.train)
autoplot(ets.fit1)

##The autoplot of ETS(A,Ad,A) is consistent with the time 
##series of the original data and implies a strong seasonal 
##component but no long term trend component. 
ets.forecast <- forecast(ets.fit1,h=48)
autoplot(ets.forecast)
checkresiduals(ets.forecast)
##The ACF plot shows 2 significant lags. Furthermore, the 
##residuals plot is very slightly skewed to the right. 
##Moreover, the Ljung-Box test produces a p-value of 
##0.00756 meaning we reject the null hypothesis of 
##no autocorrelation at 5% significance level. This means the 
##residuals do not resemble white noise and thus there 
##is information missing in this ETS model and hence 
##it is not reasonable. This is most probably because 
##the ETS model does not capture the cyclical component. 

###1f 
##seasonally-adjusted STL decomposition 
seasadjfit2 <- seasadj(stl(nyurn.train, s.window=13))
ets.fit2 <- forecast(ets(seasadjfit2),h=48)
autoplot(ets.fit2)

###1i 
fit.arima <- auto.arima(nyurn.train)
summary(fit.arima)
checkresiduals(fit.arima)
fit.arima %>% forecast %>% autoplot
##I selected the model using the auto.arima() function 
##which selects ARIMA(2,0,2)(1,1,0)[12] with drift. 
##The residuals plot shows a significant spike at 
##lag 24. Fruthermore, the Ljung-Box test produces 
##a p-value of 2.366e-05, therefore we reject the 
##null hypothesis of no autocorrelation. Since 
##the residuals do no resemble white noise, this
##selection of the ARIMA model does not look reasonable. 

###1j 
fit.nnar <- nnetar(nyurn.train)
fit.nnar %>% forecast(h=48, PI=TRUE) %>% autoplot()
checkresiduals(fit.nnar)
##There are siginficant spikes at lags 12, 24, and 36,
##and the ACF plot shows that autocorrelation is not 
##close to 0. Therefore, although the residuals plot
##is relatively normally distributed as desired,
##this model is not reasonable.

###1k 
accuracy(naivefit.nyurn,nyurn.test)
accuracy(ets.forecast, nyurn.test)
accuracy(ets.fit2,nyurn.test)
accuracy(fit.arima %>% forecast,nyurn.test)
##Based on the test mean squared error, ARIMA 
##is the best model as it has the lowest test RMSE. 

###1l 
ETS <- forecast(ets(nyurn.train),h=48)
ARIMA <- forecast(auto.arima(nyurn.train),biasadj=TRUE)
STL <- stlf(nyurn.train,biasadj=TRUE)
NNAR <- forecast(nnetar(nyurn.train),h=48)
TBATS <- forecast(tbats(nyurn.train,biasadj=TRUE),h=48)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + 
                  STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5
c(ETS=accuracy(ETS, nyurn.test)["Test set","RMSE"],
  ARIMA=accuracy(ARIMA,nyurn.test)["Test set","RMSE"],
  NNAR=accuracy(NNAR,nyurn.test)["Test set","RMSE"],
  STL.ETS=accuracy(STL,nyurn.test)["Test set","RMSE"],
  TBATS=accuracy(TBATS,nyurn.test)["Test set","RMSE"],
  Combination=
    accuracy(Combination,nyurn.test)["Test set","RMSE"])
##The combination of ETS,ARIMA, NNA, STL, TBATS 
##has the lowest RMSE compared to individual of most
##bestides STL(ETS). 


