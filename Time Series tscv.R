#install.packages("forecast",fpp2")
library(forecast)
library(fpp2)

setwd("~/Desktop/MMA/MMA867 Predictive Modelling/S3 and S4")

# ElectricPriceData<-read.csv(file.choose(), header=TRUE, sep=",")
ElectricPriceData<-read.csv("02 CSV data -- electric rates.csv")
ElectricPrice_ts <- ts(ElectricPriceData$ElectricRate,start=2004, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

f  <- function(y, h) forecast(ets(y, model="AAN"), h = h)
e1 <- tsCV(ElectricPrice_ts, f, h=1, window=60)
e1

f  <- function(y, h) forecast(ets(y, model="MMN"), h = h)
e2 <- tsCV(ElectricPrice_ts, f, h=1, window=60)
e2

f  <- function(y, h) forecast(ets(y, model="AAA"), h = h)
e3 <- tsCV(ElectricPrice_ts, f, h=1, window=60)
e3

f  <- function(y, h) forecast(ets(y, model="MMM"), h = h)
e4 <- tsCV(ElectricPrice_ts, f, h=1, window=60)
e4

plot(e1, ylab='tsCV errors')
abline(0,0)
lines(e2, col="red")
lines(e3, col="green")
lines(e4, col="blue")
legend("left", legend=c("AAN", "MMN","AAA","MMM"), col=c("black", "red", "green", "blue"), lty=1:4)

mean(abs(e1/ElectricPrice_ts), na.rm=TRUE)*100
mean(abs(e2/ElectricPrice_ts), na.rm=TRUE)*100
mean(abs(e3/ElectricPrice_ts), na.rm=TRUE)*100
mean(abs(e4/ElectricPrice_ts), na.rm=TRUE)*100

