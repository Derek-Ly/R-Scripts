library(forecast)
library(readxl)
setwd("~/Desktop/Fles/MMA/MMA867 Predictive Modelling/Assignment 2/Data/NASA")
#WFdata<-read.csv(file.choose(), header=TRUE, sep=",") #load the data

temperature<-read.csv("NASA3.csv")

str(temperature) #check the structure of the data

temperature_ts <- ts(temperature$Temp,start=1850, frequency=12)

fit <- stl(temperature_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)
summary(temperature$Temp) #examine the descriptive statistics

#Seasonality

S1 <- 24

temp_mets_msts <- msts(temperature$Temp, seasonal.periods=c(S1)) #define multiple-seasonality time series (time of day (15mins) and day of week)

#####
##### TBATS
#####

temp_met_tbats <- tbats(temp_mets_msts)
plot(temp_met_tbats) #plot decomposition
temp_met_tbats_pred <- forecast(temp_met_tbats, h=981, level=c(0.9, 0.95)) #predict 100 years out
plot(temp_met_tbats_pred, xlab="Time", ylab="Predicted Temperature, Degrees Celsius")

#####
##### A "plain vanilla" ARIMA
#####

# temp_mets_arima <- auto.arima(temp_mets_msts,seasonal=TRUE)
# temp_mets_arima_pred <- forecast(temp_mets_arima, h=981, level=c(0.9, 0.95))
# plot(temp_mets_arima_pred, xlab="Time", ylab="Predicted Temperature in Celsius")

#####
##### ARIMA on residuals
#####

mets_lm_msts <- tslm(temp_mets_msts ~ trend + season ) # Build a linear model for trend and seasonality
summary(mets_lm_msts)

residarima1 <- auto.arima(mets_lm_msts$residuals) # Build ARIMA on it's residuals
residarima1
residualsArimaForecast <- forecast(residarima1, h=981) #forecast from ARIMA
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(mets_lm_msts,h=981) #forecast from lm
regressionF <- as.numeric(regressionForecast$mean)
regressionF

forecastR <- regressionF+residualsF # Total prediction
plot(forecastR)

#Comapring regressionForecast and inclusion of arima residuals
plot(regressionForecast)
for (i in 1:981){points((i+1671+S1)/S1,forecastR[i],col="red",pch=19, cex=0.5)}

#compare with TBATS
plot(temp_met_tbats_pred, xlab="Time", ylab="Temperature in Degrees Celsius")
for (i in 1:981){points((i+1671+S1)/S1,forecastR[i],col="red",pch=19, cex=0.5)}

##### 
##### Rolling-horizon holdout: TBATS
##### 

accuracy.tbats=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:5)
{ 
nTest <- 12*i  
nTrain <- length(temp_mets_msts)- nTest - 1
train <- window(temp_mets_msts, start=1, end=1+(nTrain)/(S1))
test <- window(temp_mets_msts, start=1+(nTrain+1)/(S1), end=1+(nTrain+12)/(S1))

s <- tbats(train)
sp<- predict(s,h=12)

cat("----------------------------------
    
    Data Partition",i,"
    
    Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
    Test Set includes 10 time periods. Observations", nTrain+1, "to", nTrain+12,"
    
    ")
print(accuracy(sp,test))

accuracy.tbats<-rbind(accuracy.tbats,accuracy(sp,test)[2,5])

#print(sp$model)
}
accuracy.tbats<-accuracy.tbats[-1] 
mean(accuracy.tbats)

#####
##### Rolling-horizon holdout: ARIMA on residuals
##### 

accuracy.arima=0 # we will check average 1-day-out accuracy for 7 days
for (i in 1:5)
{ 
  nTest <- 12*i  
  nTrain <- length(temp_mets_msts)- nTest -1
  train <- window(temp_mets_msts, start=1, end=1+(nTrain)/(S1))
  test <- window(temp_mets_msts, start=1+(nTrain+1)/(S1), end=1+(nTrain+12)/(S1))
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=12)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=12)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
      
      Data Partition",i,"
      
      Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
      Test Set includes 10 time periods. Observations", nTrain+1, "to", nTrain+12,"
      
      ")
  
  print(accuracy(sp,test))
  #  print(residauto)
  
  accuracy.arima<-rbind(accuracy.arima,accuracy(sp,test)[1,5])
  
  #print(sp$model)
}
accuracy.arima<-accuracy.arima[-1]

#compare mean accuracies of the rolling holdout
mean(accuracy.tbats)
mean(accuracy.arima)

sd(accuracy.tbats)
sd(accuracy.arima)
