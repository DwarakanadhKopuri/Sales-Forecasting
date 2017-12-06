
## Cleaning the environment to read data
rm(list = ls(all=TRUE))
setwd("F:\\FD\\INSOFE class Data\\Labs\\PHD\\Machine Learning\\Regression\\EXP")
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(DMwR)
library(glmnet)
library(caret)
library(MASS)
library(vegan)
library(data.table)
library(doParallel)
library(dummies)
library(mice)
library(VIM)
library(factoextra)
library(doParallel)
library(cluster)
library(factoextra)
library(randomForest)
library(ROCR)
library(ipred)
library(pROC)
library(e1071)
library(XLConnect)
library(rpart)
library(kernlab)
library(corrplot)

## Reading train data in to environment
data_sales<-read.csv("Train.csv")

## As we are only interested in predicted women_category .So making women_data with the 
## Sales.in$ t price as name



women_data<-data_sales[data_sales$ProductCategory =="WomenClothing",]

price<-women_data[,names(women_data) %in% c("Sales.In.ThousandDollars.")]

# read the weather data,Macroeconomic data & Holiday data into the environment 
## As the weather data is in format from 2009 to 2016 .So readig worksheet from 2009 to 2016

work <-loadWorkbook("WeatherData.xlsx")

we2009 <-readWorksheet(work,"2009",header = T)

we2010 <-readWorksheet(work,"2010",header = T)

we2011 <-readWorksheet(work,"2011",header = T)
we2012 <-readWorksheet(work,"2012",header = T)
we2013 <-readWorksheet(work,"2013",header = T)
we2014 <-readWorksheet(work,"2014",header = T)
we2015 <-readWorksheet(work,"2015",header = T)
we2016 <-readWorksheet(work,"2016",header = T)

# loading the events data into environment
#
wkbk2<-loadWorkbook("Events_HolidaysData.xlsx")
event_data <-readWorksheet(wkbk2,"Sheet1",header = T)

# loading macro economic data into environment

wkbk3<-loadWorkbook("MacroEconomicData.xlsx")

macro_data<-readWorksheet(wkbk3,"Sheet1",header = T)

# check the the weather for each year and do the aggregation on each year.

str(we2009)


# convert the wind data to numeric from 2009 to 2016 and preprocessing in 
##imputation and adding few shortterms to easy way of understanding data
## And considering mostly the mean value as there won't be much vary in humidity
##on a monthly basis rom 2009 to 2016.
## Aggregating 2009 to 2016 data explict of year and month
we2009$Wind..km.h..low<- as.numeric(we2009$Wind..km.h..low)
we2009$Wind..km.h..avg<-as.numeric(we2009$Wind..km.h..avg)
we2009$Wind..km.h..high<-as.numeric(we2009$Wind..km.h..high)

# impute the missing values 

we2009<-centralImputation(we2009)

we2009$Month[we2009$Month == "Jan"] <-"01"
we2009$Month[we2009$Month == "Feb"] <-"02"
we2009$Month[we2009$Month == "Mar"] <-"03"
we2009$Month[we2009$Month == "Apr"] <-"04"
we2009$Month[we2009$Month == "May"] <-"05"
we2009$Month[we2009$Month == "Jun"] <-"06"
we2009$Month[we2009$Month == "Jul"] <-"07"
we2009$Month[we2009$Month == "Aug"] <-"08"
we2009$Month[we2009$Month == "Sep"] <-"09"
we2009$Month[we2009$Month == "Oct"] <-"10"
we2009$Month[we2009$Month == "Nov"] <-"11"
we2009$Month[we2009$Month == "Dec"] <-"12"


we2009ag<-we2009 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))

# 2010 weather data processing.
str(we2010)

we2010$Wind..km.h..high<-as.numeric(we2010$Wind..km.h..high)

we2010$Year[we2010$Year==2009] <- 2010

we2010<-centralImputation(we2010)

we2010$Month[we2010$Month == "Jan"] <-"01"
we2010$Month[we2010$Month == "Feb"] <-"02"
we2010$Month[we2010$Month == "Mar"] <-"03"
we2010$Month[we2010$Month == "Apr"] <-"04"
we2010$Month[we2010$Month == "May"] <-"05"
we2010$Month[we2010$Month == "Jun"] <-"06"
we2010$Month[we2010$Month == "Jul"] <-"07"
we2010$Month[we2010$Month == "Aug"] <-"08"
we2010$Month[we2010$Month == "Sep"] <-"09"
we2010$Month[we2010$Month == "Oct"] <-"10"
we2010$Month[we2010$Month == "Nov"] <-"11"
we2010$Month[we2010$Month == "Dec"] <-"12"

we2010ag<-we2010 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))


# 2011 weather data processing.
str(we2011)

we2011$Wind..km.h..high<-as.numeric(we2011$Wind..km.h..high)

we2011<-centralImputation(we2011)

we2011$Year[we2011$Year==2009] <- 2011

we2011$Month[we2011$Month == "Jan"] <-"01"
we2011$Month[we2011$Month == "Feb"] <-"02"
we2011$Month[we2011$Month == "Mar"] <-"03"
we2011$Month[we2011$Month == "Apr"] <-"04"
we2011$Month[we2011$Month == "May"] <-"05"
we2011$Month[we2011$Month == "Jun"] <-"06"
we2011$Month[we2011$Month == "Jul"] <-"07"
we2011$Month[we2011$Month == "Aug"] <-"08"
we2011$Month[we2011$Month == "Sep"] <-"09"
we2011$Month[we2011$Month == "Oct"] <-"10"
we2011$Month[we2011$Month == "Nov"] <-"11"
we2011$Month[we2011$Month == "Dec"] <-"12"


we2011ag<-we2011 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))
# 2012 weather data processing.
str(we2012)

we2012$Temp.high...C.<-as.numeric(we2012$Temp.high...C.)
we2012$Temp.avg...C. <-as.numeric(we2012$Temp.avg...C.)
we2012$Temp.low...C. <-as.numeric(we2012$Temp.low...C.)
we2012$Dew.Point.high...C.<-as.numeric(we2012$Dew.Point.high...C.)
we2012$Dew.Point.avg...C.<-as.numeric(we2012$Dew.Point.avg...C.)
we2012$Dew.Point.low...C.<-as.numeric(we2012$Dew.Point.low...C.)
we2012$Humidity.....high<-as.numeric(we2012$Humidity.....high)
we2012$Humidity.....avg<-as.numeric(we2012$Humidity.....avg)
we2012$Humidity.....low<-as.numeric(we2012$Humidity.....low)
we2012$Sea.Level.Press...hPa..high<-as.numeric(we2012$Sea.Level.Press...hPa..high)
we2012$Sea.Level.Press...hPa..avg<-as.numeric(we2012$Sea.Level.Press...hPa..avg)
we2012$Sea.Level.Press...hPa..low<-as.numeric(we2012$Sea.Level.Press...hPa..low)
we2012$Visibility..km..high<-as.numeric(we2012$Visibility..km..high)
we2012$Visibility..km..avg<-as.numeric(we2012$Visibility..km..avg)
we2012$Visibility..km..low<-as.numeric(we2012$Visibility..km..low)
we2012$Wind..km.h..high<-as.numeric(we2012$Wind..km.h..high)
we2012$Wind..km.h..avg<-as.numeric(we2012$Wind..km.h..avg)
we2012$Wind..km.h..low<-as.numeric(we2012$Wind..km.h..low)


we2012<-centralImputation(we2012)

we2012$Year[we2012$Year==2009] <- 2012


we2012$Month[we2012$Month == "Jan"] <-"01"
we2012$Month[we2012$Month == "Feb"] <-"02"
we2012$Month[we2012$Month == "Mar"] <-"03"
we2012$Month[we2012$Month == "Apr"] <-"04"
we2012$Month[we2012$Month == "May"] <-"05"
we2012$Month[we2012$Month == "Jun"] <-"06"
we2012$Month[we2012$Month == "Jul"] <-"07"
we2012$Month[we2012$Month == "Aug"] <-"08"
we2012$Month[we2012$Month == "Sep"] <-"09"
we2012$Month[we2012$Month == "Oct"] <-"10"
we2012$Month[we2012$Month == "Nov"] <-"11"
we2012$Month[we2012$Month == "Dec"] <-"12"

we2012ag<-we2012 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))


# 2013 weather data processing.
str(we2013)

we2013$Wind..km.h..high<-as.numeric(we2013$Wind..km.h..high)

we2013<-centralImputation(we2013)

we2013$Year[we2013$Year==2009]<- 2013


we2013$Month[we2013$Month == "Jan"] <-"01"
we2013$Month[we2013$Month == "Feb"] <-"02"
we2013$Month[we2013$Month == "Mar"] <-"03"
we2013$Month[we2013$Month == "Apr"] <-"04"
we2013$Month[we2013$Month == "May"] <-"05"
we2013$Month[we2013$Month == "Jun"] <-"06"
we2013$Month[we2013$Month == "Jul"] <-"07"
we2013$Month[we2013$Month == "Aug"] <-"08"
we2013$Month[we2013$Month == "Sep"] <-"09"
we2013$Month[we2013$Month == "Oct"] <-"10"
we2013$Month[we2013$Month == "Nov"] <-"11"
we2013$Month[we2013$Month == "Dec"] <-"12"

we2013ag<-we2013 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))


# 2014 weather data processing.
str(we2014)

we2014 <-we2014[2:nrow(we2014),]

we2014$Temp.high...C.<-as.numeric(we2014$Temp.high...C.)
we2014$Temp.avg...C.<-as.numeric(we2014$Temp.avg...C.)
we2014$Temp.low...C.<-as.numeric(we2014$Temp.low...C.)
we2014$Dew.Point.high...C.<-as.numeric(we2014$Dew.Point.high...C.)
we2014$Dew.Point.avg...C.<-as.numeric(we2014$Dew.Point.avg...C.)
we2014$Dew.Point.low...C.<-as.numeric(we2014$Dew.Point.low...C.)
we2014$Humidity.....high<-as.numeric(we2014$Humidity.....high)
we2014$Humidity.....avg<-as.numeric(we2014$Humidity.....avg)
we2014$Humidity.....low <-as.numeric(we2014$Humidity.....low)
we2014$Sea.Level.Press...hPa..high<-as.numeric(we2014$Sea.Level.Press...hPa..high)
we2014$Sea.Level.Press...hPa..avg<-as.numeric(we2014$Sea.Level.Press...hPa..avg)
we2014$Sea.Level.Press...hPa..low<-as.numeric(we2014$Sea.Level.Press...hPa..low)
we2014$Visibility..km..high<-as.numeric(we2014$Visibility..km..high)
we2014$Visibility..km..avg<-as.numeric(we2014$Visibility..km..avg)
we2014$Visibility..km..low<-as.numeric(we2014$Visibility..km..low)
we2014$Wind..km.h..low<-as.numeric(we2014$Wind..km.h..low)
we2014$Wind..km.h..avg<-as.numeric(we2014$Wind..km.h..avg)
we2014$Wind..km.h..high<-as.numeric(we2014$Wind..km.h..high)

we2014<-centralImputation(we2014)

we2014$Year[we2014$Year==2009]<- 2014



we2014$Month[we2014$Month == "Jan"] <-"01"
we2014$Month[we2014$Month == "Feb"] <-"02"
we2014$Month[we2014$Month == "Mar"] <-"03"
we2014$Month[we2014$Month == "Apr"] <-"04"
we2014$Month[we2014$Month == "May"] <-"05"
we2014$Month[we2014$Month == "Jun"] <-"06"
we2014$Month[we2014$Month == "Jul"] <-"07"
we2014$Month[we2014$Month == "Aug"] <-"08"
we2014$Month[we2014$Month == "Sep"] <-"09"
we2014$Month[we2014$Month == "Oct"] <-"10"
we2014$Month[we2014$Month == "Nov"] <-"11"
we2014$Month[we2014$Month == "Dec"] <-"12"


we2014ag<-we2014 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))


# 2015 weather data processing.
str(we2015)

we2015$Sea.Level.Press...hPa..avg<-as.numeric(we2015$Sea.Level.Press...hPa..avg)
we2015$Sea.Level.Press...hPa..high<-as.numeric(we2015$Sea.Level.Press...hPa..high)
we2015$Sea.Level.Press...hPa..low<-as.numeric(we2015$Sea.Level.Press...hPa..low)
we2015$Visibility..km..high<-as.numeric(we2015$Visibility..km..high)
we2015$Visibility..km..avg<-as.numeric(we2015$Visibility..km..avg)
we2015$Visibility..km..low<-as.numeric(we2015$Visibility..km..low)
we2015$Wind..km.h..avg<-as.numeric(we2015$Wind..km.h..avg)
we2015$Wind..km.h..low<-as.numeric(we2015$Wind..km.h..low)
we2015$Wind..km.h..high<-as.numeric(we2015$Wind..km.h..high)


we2015<-centralImputation(we2015)

we2015$Year[we2015$Year==2009]<- 2015


we2015$Month[we2015$Month == "Jan"] <-"01"
we2015$Month[we2015$Month == "Feb"] <-"02"
we2015$Month[we2015$Month == "Mar"] <-"03"
we2015$Month[we2015$Month == "Apr"] <-"04"
we2015$Month[we2015$Month == "May"] <-"05"
we2015$Month[we2015$Month == "Jun"] <-"06"
we2015$Month[we2015$Month == "Jul"] <-"07"
we2015$Month[we2015$Month == "Aug"] <-"08"
we2015$Month[we2015$Month == "Sep"] <-"09"
we2015$Month[we2015$Month == "Oct"] <-"10"
we2015$Month[we2015$Month == "Nov"] <-"11"
we2015$Month[we2015$Month == "Dec"] <-"12"

we2015ag<-we2015 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))

# 2016 weather data processing.
str(we2016)

we2016$Sea.Level.Press...hPa..high<-as.numeric(we2016$Sea.Level.Press...hPa..high)
we2016$Sea.Level.Press...hPa..avg<-as.numeric(we2016$Sea.Level.Press...hPa..avg)
we2016$Sea.Level.Press...hPa..low<-as.numeric(we2016$Sea.Level.Press...hPa..low)
we2016$Visibility..km..high<-as.numeric(we2016$Visibility..km..high)
we2016$Visibility..km..avg<-as.numeric(we2016$Visibility..km..avg)
we2016$Visibility..km..low<-as.numeric(we2016$Visibility..km..low)
we2016$Wind..km.h..low<-as.numeric(we2016$Wind..km.h..low)
we2016$Wind..km.h..avg<-as.numeric(we2016$Wind..km.h..avg)
we2016$Wind..km.h..high<-as.numeric(we2016$Wind..km.h..high)

we2016<-centralImputation(we2016)

we2016$Year[we2016$Year==2009]<- 2016


we2016$Month[we2016$Month == "Jan"] <-"01"
we2016$Month[we2016$Month == "Feb"] <-"02"
we2016$Month[we2016$Month == "Mar"] <-"03"
we2016$Month[we2016$Month == "Apr"] <-"04"
we2016$Month[we2016$Month == "May"] <-"05"
we2016$Month[we2016$Month == "Jun"] <-"06"
we2016$Month[we2016$Month == "Jul"] <-"07"
we2016$Month[we2016$Month == "Aug"] <-"08"
we2016$Month[we2016$Month == "Sep"] <-"09"
we2016$Month[we2016$Month == "Oct"] <-"10"
we2016$Month[we2016$Month == "Nov"] <-"11"
we2016$Month[we2016$Month == "Dec"] <-"12"

we2016ag<-we2016 %>% group_by(Year,Month)%>% summarise("temphigh" = mean(Temp.high...C.),
                                                       "tempavg" = mean(Temp.avg...C.),
                                                       "templow"  =mean(Temp.low...C.),
                                                       "dewhigh" = mean(Dew.Point.high...C.),
                                                       "dewavg" = mean(Dew.Point.avg...C.),
                                                       "dewlow" = mean(Dew.Point.low...C.),
                                                       "hmdyhigh" = mean(Humidity.....high),
                                                       "hmdyavg" = mean(Humidity.....avg),
                                                       "hmdylow" = mean(Humidity.....low),
                                                       "seahigh" = mean(Sea.Level.Press...hPa..high),
                                                       "seaavg" = mean(Sea.Level.Press...hPa..avg),
                                                       "sealow" = mean(Sea.Level.Press...hPa..low),
                                                       "vishigh" = mean(Visibility..km..high),
                                                       "visavg" = mean(Visibility..km..avg),
                                                       "vislow" = mean(Visibility..km..low),
                                                       "windhigh" = mean(Wind..km.h..high),
                                                       "windavg" = mean(Wind..km.h..avg),
                                                       "windlow" = mean(Wind..km.h..low))


#combine all aggregates of weather data.

we_tot_agg<-rbind(we2009ag,we2010ag,we2011ag,we2012ag,we2013ag,we2014ag,we2015ag,we2016ag)

#remove year and month from the weather aggregate data as they are there in the macro economic data.

we_tot_agg2<-we_tot_agg
we_tot_agg$Year<-NULL

we_tot_agg$Month<-NULL

#process the event data as there will be holidays or any seasonal holidays 
##in specific months which shows impact on forecasting of sales

str(event_data)

event_data$MonthDate<-as.character(event_data$MonthDate)

event_data$MonthDate
event_data$Month<-substr(event_data$MonthDate,6,7)

#create a new column count and initialize to 1 so that  we get number of holidays 
#for each month on aggregation

event_data$count<-1


event_data2<-event_data

#have only year, month and count in event data
## Hope undermentioned are not necessary as intilized to 1 
event_data$MonthDate<-NULL
event_data$Event<-NULL
event_data$DayCategory<-NULL

str(event_data)

event_data$Month<-as.numeric(event_data$Month)

#  Fill the missing months  for each year with count as 0(months having no holidays 
#are missed in the event data).

#2009 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2009,03,0))
event_data<- rbind(event_data,c(2009,08,0))

#2010 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2010,03,0))
event_data<- rbind(event_data,c(2010,08,0))

#2011 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2011,03,0))
event_data<- rbind(event_data,c(2011,08,0))

#2012 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2012,03,0))
event_data<- rbind(event_data,c(2012,08,0))

#2013 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2013,04,0))
event_data<- rbind(event_data,c(2013,08,0))

#2014 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2014,03,0))
event_data<- rbind(event_data,c(2014,08,0))


#2015 noholiday months(mar and aug)
event_data<- rbind(event_data,c(2015,03,0))
event_data<- rbind(event_data,c(2015,08,0))

#2016 noholiday month() aug)
event_data<- rbind(event_data,c(2016,08,0))


str(event_data)

event_data$count<- as.numeric(event_data$count)
# aggregate the data by year and month and holidays count.
event_agg<-event_data %>% group_by(Year,Month)%>% summarise("Holidays" = sum(count))


holiday<-event_agg$Holidays

str(holiday)

##play with micro data

colnames(macro_data)<-c("Year-Month","nom_GDP","real_GDP","CPI","party","unempy_rate",
                        "com_int_rate","per_int_rate","earnings","expenses","cot_price",
                        "chng_price","plant_area","harv_area","cot_yeild","cot_output","cot_mill_use","Exports")

# split the first column into year and month in the macro data

macro_data$year = substr(macro_data$`Year-Month`,1,4)
macro_data$month = substr(macro_data$`Year-Month`,8,10)

macro_data2<-macro_data

#drop the year-end column and party column(it has constant value democratic)

macro_data2$`Year-Month` <-NULL
macro_data2$party <-NULL

# find the attribute types of macro data

str(macro_data2)

# convert expense field to numeric.
macro_data2$expenses<-as.numeric(macro_data2$expenses)
macro_data2$month<-as.factor(macro_data2$month)
macro_data2$year<-as.numeric(macro_data2$year)

# fill the missing values by central imputation.
macro_data2<-centralImputation(macro_data2)


#combine macro data with  aggregated weather data and holiday event data 

mac_wt_data<-data.frame(macro_data2,we_tot_agg,holiday)
# standardise the numeric data 

std_method<-preProcess(mac_wt_data[,!names(mac_wt_data) %in% c("month")],
                       method = c("center","scale"))

macro_data3<-predict(std_method, mac_wt_data[,!names(mac_wt_data) %in% c("month")])

macro_data4<-mac_wt_data[,(names(mac_wt_data) %in% c("month"))]


macro_data5<-data.frame(macro_data4,macro_data3)

colnames(macro_data5)[1]= "month"

# last 12 rows of macro data correspond to 2016 data whose price need to be forecasted.
# take only data till 2015 and build the model.

macro_data6<-macro_data5[1:(nrow(macro_data5)-12),]
test_data<-macro_data5[(nrow(macro_data5)-11):nrow(macro_data5),]

#combine the price present in sales data to the macro data.
macro_data7<-data.frame(macro_data6,price)

# impute the missing values in price
macro_data7$price<-na.locf(macro_data7$price)

#corroplot


corrplot(cor(macro_data7[,!names(macro_data7) %in% c("month")]),method = "ellipse")
# get macro economic data till 2015 in a seperate dataframe so that it can be combined with
# price and used for data exploration.

macro_data_y15<-macro_data2[1:(nrow(macro_data2)-12),]
macro_data_y15<-data.frame(macro_data_y15,price)
macro_data_y15$price<-na.locf(macro_data_y15$price)

#apply the corroplot on macro economic data to check the correlation between the macro data features 
#and price.

corrplot(cor(macro_data_y15[,!names(macro_data_y15) %in% c("month")]),method = "ellipse")

# get the weather data till 2015in a seperate dataframe so that it can be combined with
# price and used for data exploration.

we_tot_agg2_y15<-we_tot_agg2[1:(nrow(we_tot_agg2)-12),]

we_tot_agg2_y15<-data.frame(we_tot_agg2_y15,price)
we_tot_agg2_y15$price<-na.locf(we_tot_agg2_y15$price)
#apply the corroplot on Weather data to check the correlation between the weather features 
#and price.

corrplot(cor(we_tot_agg2_y15[,!names(we_tot_agg2_y15) %in% c("Month")]),method = "circle")

# split the data into train and val.

train_rows <- sample(x = 1:nrow(macro_data7), size = 0.7*nrow(macro_data7))

# We use the above indices to subset the train and test sets from the data

train_data <- macro_data7[train_rows, ]

val_data <- macro_data7[-train_rows, ]


# build the model using decision tree.

## 16.14 MAPE
model_RP <-rpart(price~.,data = train_data,control = rpart.control(cp = 0.02),method = "anova")
pred_RP<-predict(model_RP,val_data)

regr.eval(val_data$price,pred_RP)

#predict on the test data:

pred_RP_test<-predict(model_RP,test_data)
pred_RP_test

write.csv(pred_agg_hday_test,"rpart_submission.csv")


# build the model using svm MAPE is 15.66%

model_svm<-svm(price~.,train_data,kernel = "linear")

pred_svm_val<-predict(model_svm,val_data)

regr.eval(val_data$price,pred_svm_val)

# predict on the test data

pred_svm_test<-predict(model_svm,test_data)
pred_svm_test

write.csv(pred_svm_test,"svm_submission.csv")

## Build model using GBM
## The best model is less mape as 6.366
## But as iterations getting runned few times the predicted 
## Values are getting changed .Variation between mape of 6.366 to 8%
library(gbm)##
model_gbm2 <- gbm(price ~ . , cv.folds = 5, interaction.depth = 4, 
                  shrinkage = 0.02, distribution= "gaussian",
                  data = train_data, n.trees = 100)

gbm.perf(model_gbm2)
pred_basic8 <- predict(model_gbm2,val_data)
regr.eval(val_data$price,pred_basic8)

# predict on the test data

pred_test8<-predict(model_gbm2,test_data)

pred_test8
write.csv(pred_test8,"gbm_submission.csv")


## If we have only macrodata then also mape is getting around 7 to 8%


