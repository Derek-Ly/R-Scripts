setwd("~/Desktop/MMA/MMA860 Acq and Manage Data/Assignment 3")

library(readxl)

wine <- read_excel("MMA_860_Assignment_3_Data_v1_0.xlsx",2)

wine$Canada<-0
wine$Canada[wine$Country == "Canada"] <- 1
wine$Canada<-as.factor(wine$Canada)

wine$Italy<-0
wine$Italy[wine$Country == "Italy"] <- 1
wine$Italy<-as.factor(wine$Italy)

wine$France<-0
wine$France[wine$Country == "France"] <- 1
wine$France<-as.factor(wine$France)

wine$US<-0
wine$US[wine$Country == "US"] <- 1
wine$US<-as.factor(wine$US)

reg1 <- lm(Rating ~Price + Alcohol + Residual_Sugar + Sulphates+ pH + Country, wine)
summary(reg1)

table(wine$Country)

#Remove Residual Sugar
reg2 <- lm(Rating ~Price + Alcohol + Sulphates+ pH + Country, wine)
summary(reg2)

#Remove pH
reg3 <- lm(Rating ~Price + Alcohol + Sulphates + Country, wine)
summary(reg3)

#Remove Country
reg4 <- lm(Rating ~Price + Alcohol + Sulphates, wine)
summary(reg4)

#Return Country and Remove Alcohol
reg5 <- lm(Rating ~Price + Sulphates + Country, wine)
summary(reg5)

#Return Alcohol and Remove Sulphates
reg6 <- lm(Rating ~Price + Alcohol+ Country, wine)
summary(reg6)

#Return Sulphates and Remove Price
reg7 <- lm(Rating ~  Alcohol+Sulphates+ Country, wine)
summary(reg7)

#Return Price
reg8 <- lm(Rating ~ Price+ Alcohol+Sulphates+ Country, wine)
summary(reg8)

reg9 <- lm(Rating ~Price + Alcohol + Sulphates+ France + Italy + US + Canada, wine)
summary(reg9)

reg10 <- lm(Rating ~ Price + Alcohol + Sulphates + France + Italy, wine)
summary(reg10)

# Question 2b)
plot(reg10)

# Question 2c)

min(wine$Alcohol)
max(wine$Alcohol)
mean(wine$Alcohol)

min(wine$Sulphates)
max(wine$Sulphates)
mean(wine$Sulphates)

rating <- -6.61457 + 0.93943*39.99  + 3.42148*11.89336  -13.79713*0.8694829 + 12.57760*1 + 9.37074*0
rating
