## Cleanining Environment
rm(list=ls(all=TRUE))
## Loading Forecasting Libraries
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
## Setting Working Directory
setwd("F:\\FD\\INSOFE class Data\\Labs\\PHD\Machine Learning")
# Read CSV file Train In to environment
sales_data <- read.csv("Train.csv")
## Checking Structure & Summary Of Data(Basic Stats Values)
str(sales_data)
summary(sales_data)

## As considering sales data and taking mean value of the target variable(Sale In Thousands)and converting Date term
sales_month <- sales_data %>% group_by(Month) %>% summarise("Sales" = mean(Sales.In.ThousandDollars.))
sales_month <- data.frame(sales_month)
sales_month$Month = as.Date(sales_month$Month,format="%Y-%m")
## Checking NA's in the data
sum(is.na(sales_data))

## Identified and imputed the reason for KNN is to consider nearer observations not the mean as the factors may vary with mean 
sales_data <- knnImputation(sales_data)
## Subsetting the product categories 
women_clothing <- subset(sales_data, ProductCategory=="WomenClothing") 
men_clothing <- subset(sales_data, ProductCategory=="MenClothing") 
other_clothing <- subset(sales_data, ProductCategory=="OtherClothing") 
## Separating the price 
women_price <- ts(women_clothing$Sales.In.ThousandDollars., frequency =12)

## Few plots for easily understanding of data and its trend 

plot(women_price,type="l",lwd=3,col="red",xlab="MONTH",ylab="Sales.In.ThousandDollars.",main="Time series plot")


women_pricedecomposed=decompose(women_price)
plot(women_pricedecomposed,col="green")

par(mfrow=c(2,2))
##one lag has stationarize the data we can use ndiffs of forecast package 
##to check no of differences required to stationarize the data
par(mfrow=c(2,3))
plot(diff(women_price,lag = 1),type="l"); acf(diff(women_price,lag = 1),lag=30) ;pacf(diff(women_price,lag = 1),lag=30)
plot(diff(women_price,lag=2),type="l");  acf(diff(women_price,lag = 2),lag=30); pacf(diff(women_price,lag = 2),lag=30)
## Auto correlatioon factor and PACF 
##The ACF can be used to estimate the MA-part, i.e q-value
##The PACF can be used to estimate the AR-part, i.e. p-value
acf(women_price,lag=30)
pacf(women_price,lag=30)

## Simple Moving Averages 
fitsma <- SMA(women_price,n=2)

predsma <- forecast(fitsma[!is.na(fitsma)],h=4)
plot(predsma)
smasalesMape <- regr.eval(women_price[2:length(women_price)],fitsma[2:length(women_price)])

smasalesMape
## Auto ARIMA 
##Returns best ARIMA model according to either AIC, AICc or BIC value. 
##The function conducts a search over possible model within the order constraints provided.
women_arima <- auto.arima(women_price)
women_arima
forecast_womenarima <- forecast(women_arima, level = c(90), h = 12)
plot(forecast_womenarima)
write.csv(forecast_womenarima,file = "women_forecast.csv")

men_price <- ts(men_clothing$Sales.In.ThousandDollars., frequency =12)


men_arima <- auto.arima(men_price)
men_arima
forecast_menarima <- forecast(men_arima, level = c(90), h = 12)
plot(forecast_menarima)
forecast_menarima


write.csv(forecast_menarima,file = "men_forecast.csv")

other_price <- ts(other_clothing$Sales.In.ThousandDollars., frequency =12)
other_arima <- auto.arima(other_price)
other_arima
forecast_otherarima <- forecast(other_arima, level = c(90), h = 12)
plot(forecast_otherarima2)
forecast_otherarima

write.csv(forecast_otherarima,file = "other_forecast.csv")
## Naives Method Just Tried As results are not showing good
men_naive <- naive(men_price)
men_naive

forecast_naivemen <- naive(men_price,h = 12,level = c(80,95),fan = FALSE,lambda = NULL)


naive(x, h=10, level=c(80,95), fan=FALSE, lambda=NULL)
##Exponential Moving Averages .AS it adds weight to the last values seen.
##As more weight is given to the latest data. 
##It's also known as the exponentially weighted moving average. 
##This type of moving average reacts faster to recent price changes than a simple moving average.

## AS GIVEN BEST RESULTS WITH 10% MAPE
rm(list=ls(all=TRUE))
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
setwd("F:\\FD\\INSOFE class Data\\Labs\\PHD\\Machine Learning")
sales_data <- read.csv("Train.csv")
str(sales_data)
summary(sales_data)
sales_month <- sales_data %>% group_by(Month) %>% summarise("Sales" = mean(Sales.In.ThousandDollars.))
sales_month <- data.frame(sales_month)
sales_month$Month = as.Date(sales_month$Month,format="%Y-%m")
sum(is.na(sales_data))
sales_data <- knnImputation(sales_data)
women_clothing <- subset(sales_data, ProductCategory=="WomenClothing") 
men_clothing <- subset(sales_data, ProductCategory=="MenClothing") 
other_clothing <- subset(sales_data, ProductCategory=="OtherClothing") 

women_price <- ts(women_clothing$Sales.In.ThousandDollars., frequency =12)



plot(women_price,type="l",lwd=3,col="green",xlab="MONTH",ylab="Sales.In.ThousandDollars.",main="Time series plot of women price")



women_pricedecomposed=decompose(women_price)
plot(women_pricedecomposed,col="green")
par(mfrow=c(2,2))
acf(women_price,lag=30)
pacf(women_price,lag=30)

fitEma <- EMA(women_price, n = 2)
predema <- forecast(fitEma[!is.na(fitEma)],h=12)
plot(predema)
predema
write.csv(predema,file = "womenex.csv")


men_price <- ts(men_clothing$Sales.In.ThousandDollars., frequency =12)
plot(men_price,type="l",lwd=3,col="blue",xlab="MONTH",ylab="Sales.In.ThousandDollars.",main="Time series plot of men price")
men_pricedecomposed=decompose(men_price)
plot(men_pricedecomposed,col="blue")
fitEma1 <- EMA(men_price, n = 2)
predema1 <- forecast(fitEma1[!is.na(fitEma1)],h=12)
plot(predema1)
predema1
write.csv(predema1,file = "menex.csv")


other_price <- ts(other_clothing$Sales.In.ThousandDollars., frequency =12)
plot(other_price,type="l",lwd=3,col="violet",xlab="MONTH",ylab="Sales.In.ThousandDollars.",main="Time series plot of other price")
other_pricedecomposed=decompose(other_price)
plot(other_pricedecomposed,col="violet")
fitEma2 <- EMA(other_price, n = 2)
predema2 <- forecast(fitEma2[!is.na(fitEma2)],h=12)
plot(predema2)
predema2
write.csv(predema2,file = "otherex.csv")







