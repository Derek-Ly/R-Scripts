#install.packages("forecast") #-- do this only once 
#Check the book: https://www.otexts.org/fpp and the blog: http://robjhyndman.com/hyndsight/forecasting/ 
library(forecast)

setwd("~/Desktop/MMA/MMA867 Predictive Modelling/S3 and S4")

#ElectricPriceData<-read.csv(file.choose(), header=TRUE, sep=",")
ElectricPriceData<-read.csv("02 CSV data -- electric rates.csv")

ElectricPrice_ts <- ts(ElectricPriceData$ElectricRate,start=2004, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

# Plot various decompositions into error/noise, trend and seasonality

fit <- decompose(ElectricPrice_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(ElectricPrice_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fit <- stl(ElectricPrice_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

plot(ElectricPrice_ts)

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
ElectricPrice_AAN <- ets(ElectricPrice_ts, model="AAN")
ElectricPrice_AAZ <- ets(ElectricPrice_ts, model="AAZ", damped=FALSE)
ElectricPrice_MMN <- ets(ElectricPrice_ts, model="MMN", damped=FALSE)
ElectricPrice_MMZ <- ets(ElectricPrice_ts, model="MMZ", damped=FALSE)

# Create their prediction "cones" for 360 months (30 years) into the future with quintile confidence intervals
ElectricPrice_AAN_pred <- forecast(ElectricPrice_AAN, h=360, level=c(0.8, 0.95))
ElectricPrice_AAZ_pred <- forecast(ElectricPrice_AAZ, h=360, level=c(0.8, 0.95))
ElectricPrice_MMN_pred <- forecast(ElectricPrice_MMN, h=360, level=c(0.8, 0.95))
ElectricPrice_MMZ_pred <- forecast(ElectricPrice_MMZ, h=360, level=c(0.8, 0.95))

# Compare the prediction "cones" visually
par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
plot(ElectricPrice_AAN_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(ElectricPrice_MMN_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(ElectricPrice_AAZ_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(ElectricPrice_MMZ_pred, xlab="Year", ylab="Predicted Electric Rate")

# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
ElectricPrice_tbats <- tbats(ElectricPrice_ts)
ElectricPrice_tbats_pred <-forecast(ElectricPrice_tbats, h=360, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(ElectricPrice_tbats_pred, xlab="Year", ylab="Predicted Electric Rate")

par(mfrow=c(1,3)) # Lets look at the three models with seasonality on one graph on the same scale
plot(ElectricPrice_AAZ_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))
plot(ElectricPrice_MMZ_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))
plot(ElectricPrice_tbats_pred, xlab="Year", ylab="Predicted Electric Rate", ylim=c(0,0.4))

par(mfrow=c(1,1)) # Lets look at them one-by-one
plot(ElectricPrice_AAZ_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(ElectricPrice_MMZ_pred, xlab="Year", ylab="Predicted Electric Rate")
plot(ElectricPrice_tbats_pred, xlab="Year", ylab="Predicted Electric Rate")

# Lets look at what our models actually are
ElectricPrice_AAZ
ElectricPrice_MMZ
ElectricPrice_tbats

# Select a model and export its predictions (mean, 80% and 95% confidence intervals) into a CSV file
write.csv(ElectricPrice_MMZ_pred, file = "Predicted Electric Rates.csv") 

#ARIMA model could also be fitted but does not work well in this case. Need ARIMA with covariates (e.g., Fourier)
ElectricPrice_arima <- auto.arima(ElectricPrice_ts, seasonal=TRUE)
ElectricPrice_arima
ElectricPrice_arima_pred <-forecast(ElectricPrice_arima, h=360)
plot(ElectricPrice_arima_pred, ylab="Predicted Electric Rate")

