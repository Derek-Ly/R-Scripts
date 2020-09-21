setwd("~/Desktop/Files/MMA/MMA860 Acq and Manage Data/Assignment 1")
library(readxl)
library(tidyverse)
library(tidyr)
library(ggplot2)

sales_data <- read_excel("MMA860_Assignment1_Data.xlsx")
head(sales_data)

# Question 2a)
sales_data$Price <- as.numeric(gsub("\\$","",sales_data$Price))
head(sales_data$Price)

sales_data$Product_ID <- str_pad(sales_data$Product_ID, width = 3, side = "left", pad = "0") 
sales_data$Product_ID

sales_data$Import <- as.factor(sales_data$Import)
str(sales_data$Import)
head(sales_data$Import)

length(sales_data)
nrow(sales_data)
str(sales_data)
sum(is.na(sales_data))

# Question 2b)

sales_data_2 <- gather(sales_data,Year,Sales,Sales_2016, Sales_2017)
sales_data_2$Year <- gsub("\\Sales_","",sales_data_2$Year)

head(sales_data_2)

# Question 2c)
hist(sales_data_2$Price, main = "Histogram of Sales Price", xlab ='Price')

# Question 2d)
ggplot(sales_data_2, aes(y= Price,x=Num_Retailers))+geom_point()+geom_smooth()
R <- (cor(sales_data_2$Num_Retailers,sales_data_2$Price))
print(R)

#cat("The correlation value is", R,". Therefore, the correlation is very low")

# Question 2e)

ggplot(sales_data_2, aes(y= Sales,x=Price))+geom_point()+geom_smooth()

#Re <- (cor(sales_data_2$Price,sales_data_2$Sales))
#print(Re)

#ggplot(sales_data, aes(Obs)) + 
 # geom_line(aes(y = Sales_2016, colour = "2016")) +
 # geom_line(aes(y = Sales_2017, colour = "2017")) +
 # labs(x="Product ID",y="Sales")

ggplot(sales_data_2, aes(y= Sales,x=Price,colour = Year))+geom_point()+geom_smooth()
#Price vs Sales for the two different years
#Maybe recession, maybe loss of reputation
