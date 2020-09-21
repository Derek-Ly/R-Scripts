#install.packages("propagate")
library(propagate)
library(readxl)


setwd("~/Desktop/MMA/MMA823 Analytics for Financial Markets/Assignment 3")
garch_vol<-read_excel("Question 3 all three methods Time Series forecasted volatility v2.xlsx",1)
agarch_vol<-read_excel("Question 3 all three methods Time Series forecasted volatility v2.xlsx",2)
ewma_vol<-read_excel("Question 3 all three methods Time Series forecasted volatility v2.xlsx",3)

#GARCH
garch_stdev <-garch_vol[2]
garch_stdev <-garch_stdev[-253,] #Dropped the last extra column since its extra 

#Asymetric GARCH
agarch_stdev <-agarch_vol[2]
agarch_stdev <-agarch_stdev[-253,] #Dropped the last extra column since its extra 

#EWMA
ewma_stdev <-ewma_vol[2]
ewma_stdev <-ewma_stdev[-253,] #Dropped the last extra column since its extra 

num_days <- 252 #number of days in the year not including weekend and holiday
sims <-10000 #Number of simulations 
mu <-0.07 
s_and_p_val <-2850.13


#GARH-----Creating a massive matrix of num_days by sims variables for GARCH
set.seed(12345)
garch_Matrix <- matrix(ncol = num_days, nrow = sims) 
garch_Matrix <- as.data.frame(garch_Matrix)
for(i in 1: ncol(garch_Matrix)){
  names(garch_Matrix)[i] <- paste("Return_Day_",i)
}

#filling in the GARCH matrix
for(i in 1: ncol(garch_Matrix)){
  for(j in 1:nrow(garch_Matrix)){
    if(i==1){
      garch_Matrix[j,i] <- s_and_p_val*exp((mu/num_days-0.5*garch_stdev$St.Dev[i]*garch_stdev$St.Dev[i]*1) + (garch_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=garch_stdev$St.Dev[i])))
    }
    else{
      garch_Matrix[j,i] <- garch_Matrix[j,i-1]*exp((mu/num_days-0.5*garch_stdev$St.Dev[i]*garch_stdev$St.Dev[i]*1) + (garch_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=garch_stdev$St.Dev[i])))
      
    }
  }
}


#Asymmetric GARCH----Creating a massive matrix of num_days by sims variables for Asymetric GARCH 
set.seed(12345)
agarch_Matrix <- matrix(ncol = num_days, nrow = sims) 
agarch_Matrix <- as.data.frame(agarch_Matrix)
for(i in 1: ncol(agarch_Matrix)){
  names(agarch_Matrix)[i] <- paste("Return_Day_",i)
}

#filling in the Asymmetric GARCH matrix
for(i in 1: ncol(agarch_Matrix)){
  for(j in 1:nrow(agarch_Matrix)){
    if(i==1){
      agarch_Matrix[j,i] <- s_and_p_val*exp((mu/num_days-0.5*agarch_stdev$St.Dev[i]*agarch_stdev$St.Dev[i]*1) + (agarch_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=agarch_stdev$St.Dev[i])))
    }
    else{
      agarch_Matrix[j,i] <- agarch_Matrix[j,i-1]*exp((mu/num_days-0.5*agarch_stdev$St.Dev[i]*agarch_stdev$St.Dev[i]*1) + (agarch_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=agarch_stdev$St.Dev[i])))
    }
  }
}


#EWMA--------Creating a massive matrix of num_days by sims variables for EWMA 
set.seed(12345)
EWMA_Matrix <- matrix(ncol = num_days, nrow = sims) 
EWMA_Matrix <- as.data.frame(EWMA_Matrix)
for(i in 1: ncol(EWMA_Matrix)){
  names(EWMA_Matrix)[i] <- paste("Return_Day_",i)
}

#filling in the EWMA matrix
for(i in 1: ncol(EWMA_Matrix)){
  for(j in 1:nrow(EWMA_Matrix)){
    if(i==1){
      EWMA_Matrix[j,i] <- s_and_p_val*exp((mu/num_days-0.5*ewma_stdev$St.Dev[i]*ewma_stdev$St.Dev[i]*1) + (ewma_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=ewma_stdev$St.Dev[i])))
    }
    else{
      EWMA_Matrix[j,i] <- EWMA_Matrix[j,i-1]*exp((mu/num_days-0.5*ewma_stdev$St.Dev[i]*ewma_stdev$St.Dev[i]*1) + (ewma_stdev$St.Dev[i] * sqrt(1)*rnorm(1, mean = mu/num_days, sd=ewma_stdev$St.Dev[i])))    
    }
  }
}

garch_Matrix_Final <- as.data.frame(garch_Matrix$`Return_Day_ 252`/garch_Matrix$`Return_Day_ 1`-1)
agarch_Matrix_Final <- as.data.frame(agarch_Matrix$`Return_Day_ 252`/agarch_Matrix$`Return_Day_ 1`-1)
EWMA_Matrix_Final <- as.data.frame(EWMA_Matrix$`Return_Day_ 252`/EWMA_Matrix$`Return_Day_ 1`-1)


#GARCH---------Histogram plots for the final annual returns
hist(garch_Matrix_Final[,1], main="GARCH - Simulations of annual returns ", probability=T,
     xlab="Annual return",col="blue",breaks=20, ylim=c(0,6),xlim = c(-0.5,0.5))
lines(density(as.numeric(garch_Matrix_Final[,1])), col=2, lwd=2) #Add trend line 

#Asymmetric GARCH---------Histogram plots for the final annual returns
hist(agarch_Matrix_Final[,1], main="Asymmetric GARCH - Simulations of annual returns ", probability=T,
     #xlab="Annual return",col="light blue",breaks=20, ylim=c(0,6),xlim = c(-0.5,0.5))
     xlab="Annual return",col="light blue")
lines(density(as.numeric(agarch_Matrix_Final[,1])), col=2, lwd=2) #Add trend line 

#EWMA---------Histogram plots for the final annual returns
hist(EWMA_Matrix_Final[,1], main="EWMA - Simulations of annual returns ", probability=T,
     #xlab="Annual return",col="light blue",breaks=20, ylim=c(0,6),xlim = c(-0.5,0.5))
     xlab="Annual return",col="yellow")
lines(density(as.numeric(EWMA_Matrix_Final[,1])), col=2, lwd=2) #Add trend line 


#Calculate variance, skew and kurtosis 
summary(garch_Matrix_Final[1])
summary(agarch_Matrix_Final[1])
summary(EWMA_Matrix_Final[1])


var(garch_Matrix_Final[1]) # 0.007386158
var(agarch_Matrix_Final[1]) # 0.007386158
var(EWMA_Matrix_Final[1]) # 0.007386158

skewness(garch_Matrix_Final[1]) # 0.09691049
skewness(agarch_Matrix_Final[1]) # 0.09691049
skewness(EWMA_Matrix_Final[1]) # 0.09691049

kurtosis(garch_Matrix_Final[1]) # 3.005437
kurtosis(agarch_Matrix_Final[1]) # 3.005437
kurtosis(EWMA_Matrix_Final[1]) # 3.005437

