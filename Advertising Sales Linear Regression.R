library(gpairs)
library(corrplot)

setwd("~/Desktop/MMA/MMA831 Marketing Analysis/Assignment 1")
sat.df <- read.csv("adv_sales.csv")

#################################################################
###### Preliminary Data Inspection

# basic descriptive statistics 
summary(sat.df)
plot(sat.df)

#
gpairs(sat.df)

hist(sat.df$store)
plot(sales~store, data=sat.df, xlab="Store", ylab="Sales Volume")

hist(sat.df$billboard)
plot(sales~billboard, data=sat.df, xlab="Billboard", ylab="Sales Volume")

hist(sat.df$printout)
plot(sales~printout, data=sat.df, xlab="Printout", ylab="Sales Volume")

hist(sat.df$sat)
plot(sales~sat, data=sat.df, xlab="Sat", ylab="Sales Volume")

hist(sat.df$comp)
plot(sales~comp, data=sat.df, xlab="Comp", ylab="Sales Volume")

hist(sat.df$price)
plot(sales~price, data=sat.df, xlab="Price", ylab="Sales Volume")

#Lets look at the correplation plot

corrplot.mixed(cor(sat.df[,c(2, 3:8)]), upper="square",lower.col = "black")

cat("Seems that billboard is our highest correlating variable, with a value of",cor(sat.df$billboard,sat.df$sales))

# Lets do a regression model with all the variables first

reg1 <- lm(sales~ store + billboard + printout + sat + comp + price, sat.df)
summary(reg1)

par(mfrow=c(2,2))
plot(reg1)
par(mfrow=c(1,1))

# Seems that the printout variable is rejected, but first let met run a regression
# between sales and printout to see if it truly has low predicting power

reg2 <- lm(sales~printout, sat.df)
summary(reg2)

# p-value for printout is extremely high, should be rejected

reg3 <- lm(sales~store + billboard + sat + comp + price, sat.df)
summary(reg3)

par(mfrow=c(2,2))
plot(reg3)
par(mfrow=c(1,1))

# Data is not heteroskedastic

## Let's begin the model building process, split data into train(75%) and test(25%) for holdout

sales_train <-subset(sat.df, X<=750)
sales_test <- subset(sat.df, X>750)

y <- sales_test$sales

Rsq <- function(reg_model){
  
 reg_pred <- (predict(reg_model, sales_test))
 SSE <- sum((y - reg_pred)^2)
 SST <- sum((y - mean(y))^2)
 rsq <- 1 - (SSE/SST)
 return(rsq)
}

## Sales ~ Price

reg4 <- lm(sales~ price, sales_train)
summary(reg4)
summary(reg4)$r.squared
summary(reg4)$adj.r.squared

rsq4 <- Rsq(reg4)

cat("For only Price, the Train R-Squared is",summary(reg4)$r.squared,"and the Test R-Squared is",rsq4)
##


## Sales ~ Price + Store

reg5 <- lm(sales~ price+store, sales_train)
summary(reg5)
summary(reg5)$r.squared
summary(reg5)$adj.r.squared

rsq5 <- Rsq(reg5)

cat("For Price and Store, the Train R-Squared is",summary(reg5)$r.squared,
    "and the Test R-Squared is",rsq5)
##

## Sales ~ Price + Store + Billboard

reg6 <- lm(sales~ price+store+billboard, sales_train)
summary(reg6)
summary(reg6)$r.squared
summary(reg6)$adj.r.squared

rsq6 <- Rsq(reg6)

cat("For Price, Store, and Billboard, the Train R-Squared is",
    summary(reg6)$r.squared,"and the Test R-Squared is",
    rsq6)
##


## Sales ~ Price + Store + Billboard + Satisfaction

reg7 <- lm(sales~ price+store+billboard+sat, sales_train)
summary(reg7)
summary(reg7)$r.squared
summary(reg7)$adj.r.squared

rsq7 <- Rsq(reg7)

cat("For Price, Store, Billboard, and Satisfaction, the Train R-Squared is",
    summary(reg7)$r.squared,"and the Test R-Squared is",
    rsq7)
##

## Sales ~ Price + Store + Billboard + Satisfaction + Competition

reg8 <- lm(sales~ price+store+billboard+sat+comp, sales_train)
summary(reg8)
summary(reg8)$r.squared
summary(reg8)$adj.r.squared

rsq8 <- Rsq(reg8)

cat("For Price, Store, Billboard, Satisfaction, and Competition, the Train R-Squared is",
    summary(reg8)$r.squared,
    "and the Test R-Squared is",rsq8)
##

## Interact billboard with store
reg9 <- lm(sales~store*billboard+store + billboard + sat + comp + price, sat.df)
summary(reg9)

summary(reg9)
summary(reg9)$r.squared
summary(reg9)$adj.r.squared

rsq9 <- Rsq(reg9)
rsq9

# I will now try interactionv variables.
# Since billboard, store, and satisfaction is our most correlating variables,
# I will focus my interactions between these three variables

## Interact satisfaction with store
reg10 <- lm(sales~store*sat+store + billboard + sat + comp + price, sat.df)
summary(reg10)

summary(reg10)
summary(reg10)$r.squared
summary(reg10)$adj.r.squared

rsq10 <- Rsq(reg10)
rsq10
## This went down, lets try billboard with satisfaction next


## Interact satisfaction with billboard
reg11 <- lm(sales~billboard*sat+store + billboard + sat + comp + price, sat.df)
summary(reg11)

summary(reg11)
summary(reg11)$r.squared
summary(reg11)$adj.r.squared

rsq11 <- Rsq(reg11)
rsq11
## This went down as well, seems like include the interaction billbaord*store brings
## our R-squared up.

