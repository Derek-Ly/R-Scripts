setwd("~/Desktop/Files/MMA/MMA860 Acq and Manage Data/Assignment 1")
library(readxl)
library(sqldf)
wealth <- read_excel("MMA860_Assignment1_Data.xlsx", sheet = 2)
head(wealth, 6)

demo <- read_excel("MMA860_Assignment1_Data.xlsx", sheet = 3)
head(demo, 6)

#Question 1a
sqldf("SELECT PRCDDA,WSWORTHWPV, max(WSWORTHWPV) FROM wealth")
#use a sub query to find the max of all, not just one

#Question 1b
sqldf("SELECT COUNT(*) from demo WHERE  STYAPT/BASHHD<0.5")

#Question 1c

sqldf("SELECT wealth.PRCDDA, BASPOP,WSWORTHWPV, min(WSWORTHWPV) FROM wealth INNER JOIN demo ON wealth.PRCDDA = demo.PRCDDA WHERE wealth.WSWORTHWPV != 0")

sqldf("SELECT wealth.PRCDDA, BASPOP,WSWORTHWPV FROM wealth INNER JOIN demo ON wealth.PRCDDA = demo.PRCDDA WHERE WSWORTHWPV = (SELECT min(WSWORTHWPV) FROM wealth)")

#Question 1d
cat("although FULL OUTTER JOIN and RIGHT JOIN is available in R, function sqldf itself, is not support these join functions")

#Question 1e
sqldf("SELECT AVG(WSDEBTB) FROM  wealth INNER JOIN demo ON wealth.PRCDDA = demo.PRCDDA WHERE ACTER < 50")

