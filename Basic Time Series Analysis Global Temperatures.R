library(forecast)
library(readxl)
setwd("~/Desktop/MMA/MMA867 Predictive Modelling/Assignment 2/Data/NASA")
#WFdata<-read.csv(file.choose(), header=TRUE, sep=",") #load the data

temperature<-read.csv("NASA3.csv")

str(temperature) #check the structure of the data

temperature_ts <- ts(temperature$Temp,start=1880, frequency=12)

fit <- stl(temperature_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)
summary(temperature$Temp) #examine the descriptive statistics

#Seasonality

#S1 <- 36

#temp_mets_msts <- msts(temperature$Temp, seasonal.periods=c(12,S1)) #define multiple-seasonality time series (time of day (15mins) and day of week)

#####
##### TBATS
#####

temperature_tbats <- tbats(temperature_ts)
temperature_tbats_pred <-forecast(temperature_tbats, h=981, level=c(0.9, 0.95))
par(mfrow=c(1,1))
plot(temperature_tbats_pred, xlab="Year", ylab="Predicted Temperature")
write.csv(temperature_tbats_pred, file = "NASA - TBATS Predicted Temperatures.csv") 

#####
##### A "plain vanilla" ARIMA
#####

temperature_arima <- auto.arima(temperature_ts, seasonal=TRUE)
temperature_arima
temperature_arima_pred <-forecast(temperature_arima, h=981, level=c(0.9, 0.95))
plot(temperature_arima_pred, ylab="Predicted Temperature")
write.csv(temperature_arima_pred, file = "NASA - ARIMA SEASONAL Predicted Temperatures.csv") 

# #####
# ##### ARIMA on residuals
# #####
# 
# mets_lm_msts <- tslm(temp_mets_msts ~ trend + season ) # Build a linear model for trend and seasonality
# summary(mets_lm_msts)
# 
# residarima1 <- auto.arima(mets_lm_msts$residuals) # Build ARIMA on it's residuals
# residarima1
# residualsArimaForecast <- forecast(residarima1, h=981) #forecast from ARIMA
# residualsF <- as.numeric(residualsArimaForecast$mean)
# 
# regressionForecast <- forecast(mets_lm_msts,h=981) #forecast from lm
# regressionF <- as.numeric(regressionForecast$mean)
# regressionF
# 
# forecastR <- regressionF+residualsF # Total prediction
# plot(forecastR)
# 
# #Comapring regressionForecast and inclusion of arima residuals
# plot(regressionForecast)
# for (i in 1:981){points((i+1671+S1)/S1,forecastR[i],col="red",pch=19, cex=0.5)}
# 
# #compare with TBATS
# plot(temp_met_tbats_pred, xlab="Time", ylab="Temperature in Degrees Celsius")
# for (i in 1:981){points((i+1671+S1)/S1,forecastR[i],col="red",pch=19, cex=0.5)}

##### 
##### Rolling-horizon holdout: Normal TBATS with no seasonality
##### 

accuracy.tbats=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:5)
{ 
  train <- window(temperature_ts, start=c(1880,1), end=c(2019-i,3))
  test <- window(temperature_ts, start=c(2019-i,4), end=c(2019-i+1,3))

s <- tbats(train)
sp<- predict(s,h=12)

cat("----------------------------------
    
    Data Partition",i,"
  
    ")
print(accuracy(sp,test))

accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,5])

#print(sp$model)
}
accuracy.tbats<-accuracy.tbats[-1] 
mean(accuracy.tbats)

#####
##### Rolling-horizon holdout: Normal ARIMA with no seasonality
##### 

accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:5)
{ 

  train <- window(temperature_ts, start=c(1880,1), end=c(2019-i,3))
  test <- window(temperature_ts, start=c(2019-i,4), end=c(2019-i+1,3))
  
  s <- auto.arima(train,seasonal = TRUE)
  sp<- forecast(s,h=12)
  
  cat("----------------------------------
      
      Data Partition",i,"
      ")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[2,5])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]

#compare mean accuracies of the rolling holdout
mean(accuracy.tbats)
mean(accuracy.arima)

sd(accuracy.tbats)
sd(accuracy.arima)
