library(dplyr)
library(ggplot2)
library(readr)
library(extrafont)
library(ggthemes)
library(caret)
library(cluster) # For preprocessing
library(factoextra) # for elbow plots
library(amap) # For distance metrics
library(stylo) # For cosine distance
loadfonts(quiet = T)
library(gridExtra) # For the grid plot for ggplot2
library(dbscan) # Using dbscan
library(tsne)
library(reticulate)
library(klaR) # For K-Modes
library(clustMixType) # For K Prototype Clustering

setwd("~/Desktop/MMA/MMA869 Machine Learning and AI/Team Project/Working")
# setwd("~/MMA869")

full <- read.csv('final_data.csv')

full_no_zero <- full[full$Value != 0,] #Remove all the rows with zero
nrow(full) - nrow(full_no_zero) # removed 28473 customers

full_no_zero$Customer_ID <- NULL
dat <- full_no_zero

# plotClust = function(df, colorCol) {
#   ggplot(df, aes(x= Dwell_Time, y=Value)) + 
#     geom_point(aes(color=factor(colorCol)), size=1) +
#     geom_rangeframe() + 
#     theme_tufte(base_family="Calibri") + 
#     #theme(legend.position="none") + 
#     theme(axis.title.x = element_text(colour="grey20",size=15,face="plain")) + 
#     theme(axis.title.y = element_text(colour="grey20",size=15,face="plain")) + 
#     theme(axis.text.x = element_text(colour="grey20",size=10,face="plain")) + 
#     theme(axis.text.y = element_text(colour="grey20",size=10,face="plain")) +
#     labs(x = "Dwell_Time", y = "Value") 
# }  

iwidth = 10
iheight = 7

theme_set(theme_gray(base_size = 32))

# ggplot(dat, aes(x=Dwell_Time, y=Value)) + 
#   geom_point(color="black", size=3) +
#   geom_rangeframe() + 
#   theme_tufte(base_family="Calibri") + 
#   theme(legend.position="none") + 
#   theme(axis.title.x = element_text(colour="grey20",size=20,face="plain")) + 
#   theme(axis.title.y = element_text(colour="grey20",size=20,face="plain")) + 
#   theme(axis.text.x = element_text(colour="grey20",size=16,face="plain")) + 
#   theme(axis.text.y = element_text(colour="grey20",size=16,face="plain")) +
#   labs(x = "Dwell Time", y ="Value")

pre = preProcess(dat[,c("Value", "Dwell_Time", "Unique_Categories","Day_Preference")], method="range")
dat.scaled = predict(pre, dat, method=c("range"))
head(dat)
head(dat.scaled)
# dat.scaled %>%
#   select(Value, Dwell_Time, Unique_Categories,Day_Preference) %>%
#   head(n=15)

df = dat.scaled[,c("Value", "Dwell_Time", "Unique_Categories","Day_Preference")]

wss<-vector()
for (i in 2:15){wss[i] <- sum(kproto(df, i)$withinss)}
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# kpro <- kproto(df, 4)
clprofiles(kpro, df)

df$cluster <- kpro$cluster

par(mfrow=c(1,1))

plot(df[,c(2,1)], col=df$cluster, main="K-prototypes") # Dwell Time
plot(df[,c(3,1)], col=df$cluster, main="K-prototypes") # Unique Categories
plot(df[,c(4,1)], col=df$cluster, main="K-prototypes") # Day Preference

# Ballers we can get them to widen their focus
# Cluster 2 are opportunistic shoppers, they are susceptible to ads
