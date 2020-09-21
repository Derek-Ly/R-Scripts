# Question 2
setwd("~/Desktop/Files/MMA/MMA860 Acq and Manage Data/Assignment 2")
library(readxl)

my_data <- read_excel("MMA_860_Assignment_2_Data_v1_0.xlsx")

plot(my_data)

# Question 2a
my_data_25 <- head(my_data,25)

# Question 2a.i)
reg1 <- lm(Y ~ Experience + Height, my_data_25)
print(reg1)
summary(reg1)

reg2 <- lm(Y ~ Experience + Height, my_data)
print(reg2)
summary(reg2)

# There does seem to be a correlation between height and Y. 
# The P-Value for the F-test is quite low, 
# therefore the model does seem to show a correlation between the data for both sets of data 
# The P-Value for the t-test for the Height is quite low if the alpha is assumed to be 0.05.
# Therefore, there does seem to a correlation between height and Y for both sets of data.
# It was noted that the t-test P-value Height variable for the dataset with all of the 
# observations is much lower, and therefore has a higher predictive power. This is logical,
# As an increase of data can improve the prediction model.

# Question 2a.ii)

reg3 <- lm(Y ~ Experience + Weight, my_data_25)
print(reg3)
summary(reg3)

reg4 <- lm(Y ~ Experience + Weight, my_data)
print(reg4)
summary(reg4)

# There does seem to be a correlation between weight and Y. 
# The P-Value for the F-test is quite low, therefore the model does seem to show a correlation 
# between the data for both data sets.
# Therefore, there does seem to a weak correlation between weight and Y for both data sets.
# It was noted that the t-test P-value Weight variable for the dataset with all of the 
# observations is much lower, and therefore has a higher predictive power. This is logical,
# As an increase of data can improve the prediction model.

# Question 2a.iii)

reg5 <- lm(Y ~ Experience + Weight + Height, my_data_25)
print(reg5)
summary(reg5)

reg6 <- lm(Y ~ Experience + Weight + Height, my_data)
print(reg6)
summary(reg6)

# The summary of the regression model to explain Y for Experience, Height, and Weight
# notes that the P-Value for the F-test is quite low, therefore the model does 
# seem to show a correlation between the data for both data sets. Comparing the two datasets,
# The increase of observations illustrates an increase of significance for Height, and a decrease
# of significance for Weight. That is, the p-value for the Height decreases and increases for Weight,
# as observations increase. This is logical, as an increase of relevant data will strengthen a regression model
# paint a more accurate statistical picture

# Question 2b

# Y vs. Experience for first 25 observations
reg7 <- lm(Y ~ Experience, my_data_25)
print(reg7)
summary(reg7)

# Y vs. Experience for all of the observations
reg8 <- lm(Y ~ Experience, my_data)
print(reg8)
summary(reg8)

# Y vs. Height for first 25 observations
reg9 <- lm(Y ~ Height, my_data_25)
print(reg9)
summary(reg9)

# Y vs. Weight for all of the observations
reg10 <- lm(Y ~ Height, my_data)
print(reg10)
summary(reg10)

# Y vs. Height for first 25 observations
reg11 <- lm(Y ~ Weight, my_data_25)
print(reg11)
summary(reg11)

# Y vs. Weight for all of the observations
reg12 <- lm(Y ~ Weight, my_data)
print(reg12)
summary(reg12)

# All of the variables appear to be significant, with the exception of Height and Weight when
# running a regression model for all observations. It appears that with the exclusion
# of the Experience vaiable, increasing the observations decreased the predictive power
# for the variables Height and Weight

# Question 2c

# Linear regression models were ran for Y vs. Weight and Y vs. Height. Although both of the
# individual regression models ran for each of these variables noted that they individually correspond with Y,
# Running a joint F-test notes that variables Height and Weight are insignificant, while
# The F-test concludes a significant model (low p-value). In this situation, it can be assumed that
# Height and Weight are collinear, where one predictor variable can explain another variable,
# thus showing the same trend. We cannot mistake correlation with causation. We should assess
# Both variables and their respective t-test p-value, and decide which variable is the most significant.