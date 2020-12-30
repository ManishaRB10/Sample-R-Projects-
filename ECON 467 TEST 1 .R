###ECON 467
library(fpp2)
library(seasonal)

###Question 1 
data1=read.csv("data1.csv")
data1=ts(data1$x,start=c(2010,1),freq=12)
autoplot(data1)
###1a) 
fit1 <- seas(data1, x11="")
autoplot(fit1)
autoplot(data1, series="Data") + 
  autolayer(trendcycle(fit1), series="Trend") +
  autolayer(seasadj(fit1), series="Seasonally Adjusted") +  
  xlab("Year") + ylab("New orders index") + 
  ggtitle("Data 1 X11 Decomposition")
##X11 decomposition 
##There is an increasing trend in the data.
##The decomposition is multiplicative because as the seasonal component 
##and time plot shows, there are slight flunctuations in the magnitude
##of the seasonal changes. For example, looking closely at 
##the seasonal component, we can see that the dips in data 
##are deeper towards the end than in the beginning.

###1b) 
##STL Decomposition
fit2 <- stl(data1, s.window=13)
autoplot(fit2) + ggtitle("STL Decomposition of Data 1")
autoplot(data1, series="Data") + 
  autolayer(trendcycle(fit2), series="Trend") +
  autolayer(seasadj(fit2), series="Seasonally Adjusted") +  
  xlab("Year") + ylab("New orders index") + 
  ggtitle("Data 1 STL Decomposition")
##This STL decomposition is additive. In order to get 
##multiplicative we would have to take logs. 

##Since there is non-constant seasonal variation,
##we can use log-transformed time series like below. 
fit2.seasonal = seasonal(fit2)
ggsubseriesplot(fit2.seasonal)
logX = log(data1)
logX.stl = stl(logX,s.window=13)
autoplot(logX.stl) + ggtitle("STL Decomposition of log(Data 1)")
##this is a addtive STL decomposition of log(Data 1) 
##which is equivalent to a multiplicative decomposition. 

###1c) 
##seasonally adjusted data from STL 
autoplot(data1, series="Data") + 
  autolayer(seasadj(fit2), series="Seasonally Adjusted")

###1d) 
seasadjfit2 <- seasadj(fit2)
##check RSME for each method 
##drift method 
e <- tsCV(seasadjfit2,rwf,drift=TRUE,h=24)
sqrt(mean(e^2,na.rm=TRUE))
##mean method 
e <- tsCV(seasadjfit2,meanf,h=24)
sqrt(mean(e^2,na.rm=TRUE))
##naive method 
e <- tsCV(seasadjfit2,rwf,h=24)
sqrt(mean(e^2,na.rm=TRUE))
##seasonal naive method 
e <- tsCV(seasadjfit2,snaive,h=24)
sqrt(mean(e^2,na.rm=TRUE))
##Naive method has the smallest RSME computed using time series 
##cross-validation so I will be using this forecasting model.
naivefit.data1 <- naive(seasadjfit2,h=24)
autoplot(naivefit.data1)

###1e)
res.naive <- residuals(naivefit.data1)
checkresiduals(naivefit.data1)
##The residuals from the naive plot do not resemble 
##white noise as demonstrated by the ACF plot and 
##Ljung-Box test. Therefore, they do not satisfy 
##the desirable property of no autocorrelation. 
##There appears to be relatively constant variance from 
##the residuals plot, and close to a normal distribution
##which is desirable. However, since the residuals do not 
##ultimately resemble white noise, there is considerable 
##information remaining in the residuals that has not 
##been captured by the naive method. 

###Question 2 
data2=read.csv("data2.csv")
data2=ts(data2$x,start=c(1995,1),freq=4)

###2a)
autoplot(data2)
##split into test and train data 
data2.train <- window(data2,end=c(2014,4))
data2.test <- window(data2,start=c(2015,1))

###2b)
##The times series plot of the data shows that 
##there is no trend nor is there a clear 
##seasonal pattern. Furthermore, the data 
##is not strictly positive so a model with 
##multiplicative errors would not be useful. 
##Therefore, ETS(A,N,N) would be the best 
##since it uses additive errors and takes into 
##account the lack of a trend component and 
##seasonal componenet. 

###2c) 
ets.fit1 <- ets(data2.train, model="ANN")
autoplot(ets.fit1)
##The autoplot of ETS(A,N,N) is representative 
##of the data we observed initially in the time 
##series plot of the original data. The seasonal 
##and slope component are omitted. Furthermore,
##there is no pattern in levels. Therefore, I 
##think this method will be most appropiate. 

###2d) 
ets.fit2 <- ets(data2.train, model="AAA",damped=TRUE)
autoplot(ets.fit2)
##The autoplot of ETS(A,Ad,A) indicates that there is 
##a seasonal trend in the data which is disproved by 
##the time series plot of the original data set. 
##The slope component indicates an exponential increase 
##then remains constant. Overall, this model does not 
##fit the data well. 

###2e) 
##ets.fit2 <- ets(data2.train, model="MAM")
##this line produces a error hence it is not possible 
##to apply this model because of the negative values 
##in the dataset. A multiplicative error model 
##is not suitable for negative values.

###2f) 
accuracy(ets.fit1)
accuracy(ets.fit2)
##According to the RSME using just the training data, 
##ETS(A,Ad,A) fits the data best. This is because 
##this model is the more complex one, and more complex 
##models always fit the training data better. However,
##this is no indication that the test error for 
##this method will also be smaller. 

###2g) 
##The most appropiate model is ETS(A,N,N) because of the 
##charactersistics observed in the plots of the models 
##as well as times series plot of the original data. 
##This is further verified by the function ets(), which 
##automatically chooses ETS(A,N,N) for data2.train in 
##its default mode 
ets(data2.train)

##ETS(A.N,N)
ets.fit3 <- ets(data2.train, model="ANN")
autoplot(ets.fit3)
ets3_train <- forecast(ets.fit3, h=20)
autoplot(ets3_train)
checkresiduals(ets3_train)
##The ACF plot resembles white noise therefore 
##satisfying desirable properties. 
##The residuals are close to normal distribution 
##and the ACF plot shows there is no autocorrelation. 
##The Ljung-Box test also has a p-value of 0.2978 
##meaning we fail to reject the null hypothesis of 
##no autocorrelation at 5% significance level. 

###2h)
#c 
ets1_train <- forecast(ets.fit1, h=20)
accuracy(ets1_train, data2.test)
##Test RMSE=1.5597766
#d 
ets2_train <- forecast(ets.fit2, h=20)
accuracy(ets2_train, data2.test)
#Test RMSE=1.5975088
#e 
##Not Available since we apply ETS(M,A,M) to this data set.
#g(same as c)
accuracy(ets3_train, data2.test)
##Test RMSE=1.5597766

##ETS(A,N,N) performs the best because it has 
##the lowest test RMSE. As mentioned earlier, 
##the original data set does not portray any trends 
##or seasonality which is why this model is the most 
##appropiate. In addition, there are negative values 
##in the dataset which is why multiplicative error 
##would be inappropiate. 












