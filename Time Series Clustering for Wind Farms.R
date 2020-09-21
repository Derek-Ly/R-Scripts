library(TSclust)
library(readxl)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(matrixStats)
library(stats)

setwd("~/Desktop/MMA/MMA823 Analytics for Financial Markets/Team Project")
prices <- read.csv("nodal_prices.csv")
node_location <- read_excel("node_location.xlsx")
generators <- read_excel("generators2.xlsx")
str(prices)

prices <- prices[,1:171]

datetime <- prices$DATETIME
prices$DATETIME <- NULL

avg_nodal_price <- colMeans(as.matrix((prices)))

symbols <- colnames(prices)
prices <-as.matrix(prices)
rownames(prices) <- datetime
prices <- t(prices)
prices <- as.matrix(prices)

### Function for finding the average prices for each cluster

avgclusterprice <- function(corr_cluster,avg_nodal_price){
  i <- 1
  avg_cluster<-NULL
  df <- cbind(corr_cluster,avg_nodal_price)
  while(i <= max(corr_cluster)){
    avg_cluster[i] <- mean(avg_nodal_price[corr_cluster==i])
    i = i+1;
  }
  print(avg_cluster)
}

### Clustering begins here

D1 <- diss(prices, "COR")
summary(D1)
dst <- data.matrix(D1)
dim <- ncol(dst)
image(1:dim, 1:dim, dst, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim, symbols[1:length(symbols)], cex.axis = 0.5, las=3)
axis(2, 1:dim, symbols[1:length(symbols)], cex.axis = 0.5, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex=0.6)
sort(rowMeans(as.matrix(D1))) # Which nodes present the most unique time series? The higher, the more differ

# Running some hierarchy clustering
C1 <- hclust(D1)
plot(C1)

plot(pam(D1, k=8))
plot(pam(D1, k=7))
plot(pam(D1, k=6))
plot(pam(D1, k=5))
plot(pam(D1, k=4))
plot(pam(D1, k=3))
plot(pam(D1, k=2))

corr_cluster5 <- cutree(C1, 5)
corr_cluster7 <- cutree(C1, 7)

corr_cluster10 <- cutree(C1, 10)
corr_cluster15 <- cutree(C1, 15)

avgclusterprice(corr_cluster5,avg_nodal_price)
avgclusterprice(corr_cluster7,avg_nodal_price)

cluster_location_corr <- cbind(node_location,avg_nodal_price,corr_cluster5,corr_cluster7)

prices_corr_5 <- as.data.frame(cbind(prices,corr_cluster5))
price_corr5_1mean <- colMeans(prices_corr_5[prices_corr_5$corr_cluster5==1,])
price_corr5_1std <- colSds(as.matrix(prices_corr_5[prices_corr_5$corr_cluster5==1,]))
price_corr5_2mean <- colMeans(prices_corr_5[prices_corr_5$corr_cluster5==2,])
price_corr5_2std <- colSds(as.matrix(prices_corr_5[prices_corr_5$corr_cluster5==2,]))
price_corr5_3mean <- colMeans(prices_corr_5[prices_corr_5$corr_cluster5==3,])
price_corr5_3std <- colSds(as.matrix(prices_corr_5[prices_corr_5$corr_cluster5==3,]))
price_corr5_4mean <- colMeans(prices_corr_5[prices_corr_5$corr_cluster5==4,])
price_corr5_4std <- colSds(as.matrix(prices_corr_5[prices_corr_5$corr_cluster5==4,]))
price_corr5_5mean <- colMeans(prices_corr_5[prices_corr_5$corr_cluster5==5,])
price_corr5_5std <- colSds(as.matrix(prices_corr_5[prices_corr_5$corr_cluster5==5,]))

plot.ts(price_corr5_1mean,col = "blue")
lines(price_corr5_1mean+1*price_corr5_1std,col = "red",lwd=0.6, lty=2)
lines(price_corr5_1mean-1*price_corr5_1std,col = "red",lwd=0.6, lty=2)

# Cluster 3 is California
# Custer 4 is Texas

# Fall [1 : 2207]
# Winter [2208 : 4344]
# Spring [4345 : 6567]
# Summer [6577 : 8760]

t1 <- 1
t2 <- 8760
plot.ts(price_corr5_3mean[t1:t2],col = "blue")
lines(price_corr5_3mean[t1:t2]+1*price_corr5_3std[t1:t2],col = "red",lwd=0.6, lty=2)
lines(price_corr5_3mean[t1:t2]-1*price_corr5_3std[t1:t2],col = "red",lwd=0.6, lty=2)

mean(price_corr5_3mean)

# usa <- map_data("usa")
# cn <- map_data("world","china")
# canada <- map_data("world", "Canada")
# 
# ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
#   geom_point(data = generators, aes(x=Longitude, y = Latitude))+
#   coord_fixed(1.5)

### EHI DONT RUN THIS, IT TAKES FOREVER

# Frechet Distance
# Difference between two curves (or two time series)
D2 <- diss(prices, "FRECHET")
summary(D2)

dst2 <- data.matrix(D2)
dim2 <- ncol(dst2)
image(1:dim2, 1:dim2, dst2, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim2, symbols[1:length(symbols)], cex.axis = 0.5, las=3)
axis(2, 1:dim2, symbols[1:length(symbols)], cex.axis = 0.5, las=1)
text(expand.grid(1:dim2, 1:dim2), sprintf("%0.1f", dst2), cex=0.6)

# Two branches
# Branch 1: Google and Amazon
# Branch 2: Everything else, where LMT, IBM, and BA close together

# Now plot the hierarchy clustering for frechet

C2 <- hclust(D2)
plot(C2)

### YOU CAN RUN THIS THOUGH

D3 <- diss(prices, "DTWARP") # Dynamic Time Warping Distance
# comparing time series where the timing or the tempo of the variations may vary between the series
summary(D3)

dst3 <- data.matrix(D3)
dim3 <- ncol(dst3)
image(1:dim3, 1:dim3, dst3, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim3, symbols[1:length(symbols)], cex.axis = 0.5, las=3)
axis(2, 1:dim3, symbols[1:length(symbols)], cex.axis = 0.5, las=1)
# text(expand.grid(1:dim3, 1:dim3), sprintf("%0.1f", dst3), cex=0.6)
sort(rowMeans(as.matrix(D3)))

# PAM Clustering Silhouette Plot
plot(pam(D3, k=8))
plot(pam(D3, k=7))
plot(pam(D3, k=6))
plot(pam(D3, k=5))
plot(pam(D3, k=4))
plot(pam(D3, k=3))
plot(pam(D3, k=2))

C3 <- hclust(D3)
plot(C3)

dtwarp_cluster2 <- cutree(C3, 2)
dtwarp_cluster3 <- cutree(C3, 3)

dtwarp_cluster10 <- cutree(C3, 10)
dtwarp_cluster15 <- cutree(C3, 15)

avgclusterprice(dtwarp_cluster2,avg_nodal_price)
avgclusterprice(dtwarp_cluster3,avg_nodal_price)

cluster_location <- cbind(node_location,corr_cluster,dtwarp_cluster2,dtwarp_cluster3,avg_nodal_price)

# Integrated Periodogram

D4 <- diss(prices, "INT.PER")

# integrated Periodogram is a variation of the periodogram where the power is accumulated as a function of frequency
# This is a more robust measure for the purposes of comparing spectra
# Signals with comparable integrated periodograms will contain variations at similar frequencies.

summary(D4)

dst4 <- data.matrix(D4)
dim4 <- ncol(dst4)
image(1:dim4, 1:dim4, dst4, axes = FALSE, xlab="", ylab="")
axis(1, 1:dim4, symbols[1:length(symbols)], cex.axis = 0.5, las=3)
axis(2, 1:dim4, symbols[1:length(symbols)] , cex.axis = 0.5, las=1)
text(expand.grid(1:dim4, 1:dim4), sprintf("%0.1f", dst4), cex=0.6)

C4 <- hclust(D4)
plot(C4)

# PAM Clustering Silhouette Plot
plot(pam(D4, k=12))
plot(pam(D4, k=11))
plot(pam(D4, k=10))
plot(pam(D4, k=9))
plot(pam(D4, k=8))
plot(pam(D4, k=7))
plot(pam(D4, k=6))
plot(pam(D4, k=5))
plot(pam(D4, k=4))
plot(pam(D4, k=3))
plot(pam(D4, k=2))

ip_cluster9 <- cutree(C4, 9)

ip_cluster10 <- cutree(C4, 10)
ip_cluster15 <- cutree(C4, 15)

avgclusterprice(ip_cluster9,avg_nodal_price)

cluster_location <- cbind(node_location,
                          corr_cluster,
                          dtwarp_cluster2,
                          dtwarp_cluster3,
                          ip_cluster9,
                          avg_nodal_price)

cluster_location_2 <- cbind(node_location,
                          corr_cluster10,
                          corr_cluster15,
                          dtwarp_cluster10,
                          dtwarp_cluster15,
                          ip_cluster10,
                          ip_cluster15,
                          avg_nodal_price)

# Expore the locations for Craig to put onto tableau
# write.csv(cluster_location, "cluster_location.csv")
# write.csv(corr_cluster7, "corr_7.csv")

# write.csv(cluster_location_2, "cluster_location_2.csv")

# avgclusterprice(dtwarp_cluster15,avg_nodal_price)
