#Team Blue Jays 

library(ROSE) #ROSE package for Up or down sampling
library(forecast)
library(tidyr)
library(tidyverse)
library(plotly)
library(readxl)
library(psych)
library("ggcorrplot") #user ggcorrplot library
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library(gtools) # for discretisation
library(corrplot)
library(Hmisc)
library(devtools)
library(PerformanceAnalytics)
library(FactoMineR)
library(readxl)
library(RColorBrewer)
library(gridExtra)
library(Metrics)

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071","randomForest","xgboost","corrplot","ggthemes","devtools","PerformanceAnalytics","FactoMineR","Hmisc","tableplot") 
pacman::p_load("partykit","rpart","nnet",'Metrics',"frame")

setwd("~/Desktop/MMA/MMA823 Analytics for Financial Markets/Assignment 2")

bkr_data <- read_excel("Bankruptcy_data_Final.xlsx") 
#feature_NAs <- print(count(sapply(bkr_data, anyNA)))
bkr_data$BK <- as.factor(bkr_data$BK)
bkr_data$DataYearFiscal <- as.factor(bkr_data$DataYearFiscal)

bkr_data <- as.data.frame(cbind(bkr_data[c(1,15)],scale(bkr_data[,2:14],center =TRUE, scale = TRUE)))
#EDA
table(bkr_data$BK)
plot(bkr_data$BK)

glimpse(bkr_data)

#Selecting list of columns with missing values 
for (i in 1 : ncol(bkr_data)){
    if (any(is.na(bkr_data[,i]))){
      print(paste(names(bkr_data[i]),sum(is.na(bkr_data[,i]))))
    }
}

#Imputing missing values with 0 and creating a surrogate feature with a binary indicator
integer_reac<- 0
for (i in 1 : ncol(bkr_data)){
  if (any(is.na(bkr_data[,i]))){
      bkr_data[is.na(bkr_data[,i]),i]<-integer_reac
      #bkr_data[,paste0(colnames(bkr_data)[i],"_surrogate")]<- as.factor(ifelse(is.na(bkr_data[,i]),"1","0"))
  }
  else{
bkr_data
  }
} 

data_cor <-round(cor(bkr_data[c(2:13)]),3) #same thing as line 85, just better version
ggcorrplot(data_cor,hc.order = TRUE,  type="full", lab=TRUE, title = "Data Correlation")+theme(plot.title = element_text(hjust = 0.5))

pairs.panels(bkr_data[2:6], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


#Run the model without any upsampling or down sampling to observe BK=1
inTrain <- createDataPartition(y = bkr_data$BK,
                               p = 0.75, list = FALSE)

bkr_training <- bkr_data[ inTrain,]
bkr_testing <- bkr_data[ -inTrain,]


#---------------------Upsampling the the Majority class BK=1-----
#newData <- ovun.sample(BK ~ ., data = bkr_training, method = "over" N = 184658)$bkr_data

newData <- ovun.sample(BK ~ ., data = bkr_data, method = "over", p=0.40, seed=1)
bkr_data_sample <- newData$data
table(bkr_data_sample$BK)

#Run the model without any upsampling or down sampling to observe BK=1
inTrain_smpl <- createDataPartition(y = bkr_data_sample$BK,
                                    p = 0.75, list = FALSE)

bkr_training_smpl <- bkr_data_sample[ inTrain_smpl,]
bkr_testing_smpl <- bkr_data_sample[ -inTrain_smpl,]

table(bkr_training_smpl$BK)
table(bkr_testing_smpl$BK)

#--------------------------METHOD 2:CART----------------
# There are two families of CART algorithms: conditoinal interence trees (ctree function; caret package) and recursive partitioning (rpart function; partykit package)


bkr_ctree<-ctree(BK~ . ,data=bkr_training) #Run ctree on training data
plot(bkr_ctree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

bkr_ctree_probs<-predict(bkr_ctree,newdata=bkr_testing,type="prob") #Predict probabilities
bkr_ctree_classfication<-rep("1",18573)
table(bkr_ctree_classfication)
view(bkr_ctree_probs)
table(bkr_ctree_probs[,1])
table(bkr_ctree_probs)
bkr_ctree_classfication[bkr_ctree_probs[,1]>0.50]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
#ctree_classification <- as.integer(ctree_probabilities > mean(train_credit_data$default_0 == "1"))
bkr_ctree_classfication<-as.factor(bkr_ctree_classfication)
table(bkr_ctree_classfication)
###Confusion matrix  
confusionMatrix(bkr_ctree_classfication,bkr_testing$BK,positive = "1")


####ROC Curve
bkr_ctree_probs_testing <-predict(bkr_ctree,newdata=bkr_testing,type = "prob") #Predict probabilities
bkr_ctree_pred_testing <- prediction(bkr_ctree_probs_testing[,2], bkr_testing$BK) #Calculate errors
bkr_ctree_ROC_testing <- performance(bkr_ctree_pred_testing,"tpr","fpr") #Create ROC curve data
par(mfrow=c(1,1))

plot(bkr_ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(bkr_ctree_pred_testing,"auc") #Create AUC data
bkr_ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
bkr_ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(bkr_ctree_probs[,2],  bkr_testing$BK, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
table(bkr_data$BK)
#-----------------------------METHOD 3:Random Forest----------------
#Memory issue cant run locally
memory.limit()
sessionInfo()
model_forest <- randomForest(BK~ ., data=bkr_training_smpl, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily
print(model_forest)   
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=bkr_testing_smpl,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",213)
forest_classification[forest_probabilities[,2]<0.50]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,bkr_testing_smpl$BK, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], bkr_testing_smpl$BK) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  OJ_testing$Purchase, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)

#---------------------------------METHOD 4: Logistic Regression ----------
model_logistic<-glm(BK~ ., data=bkr_training_smpl, family="binomial"(link="logit"))

summary(model_logistic$effects) 
plot(model_logistic$weights)
fit_bkr_default_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(fit_bkr_default_stepwiseAIC)

par(mfrow=c(1,4)) # this command sets the plot window to show 1 row of 4 plots
plot(fit_bkr_default_stepwiseAIC) #diagnostic plots for the "fit" model
plot(density(resid(fit_bkr_default_stepwiseAIC)))

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(fit_bkr_default_stepwiseAIC,newdata=bkr_testing_smpl,type="response") #Predict probabilities
logistic_classification<-rep("1",nrow(bkr_testing_smpl))
logistic_classification[logistic_probabilities<0.50]="0" 

logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
Sampled_conf<-confusionMatrix(logistic_classification,bkr_testing_smpl$BK,positive = "1") #Display confusion matrix

par(mfrow=c(1,1)) # this command sets the plot window to show 1 row of 4 plots

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, bkr_testing_smpl$BK)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value


#Testing the unsampled data Test data set
logistic_probabilities_test<-predict(fit_bkr_default_stepwiseAIC,newdata=bkr_testing,type="response") #Predict probabilities
view(round(logistic_probabilities_test,3))
logistic_classification_test<-rep("1",nrow(bkr_testing))
logistic_classification_test[logistic_probabilities_test<0.50]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
table(logistic_classification_test)
view(logistic_classification)

logistic_classification_test<-as.factor(logistic_classification_test)

###Confusion matrix  
UnSampled_conf <-confusionMatrix(logistic_classification_test,bkr_testing$BK,positive = "1") #Display confusion matrix

par(mfrow=c(1,1)) # this command sets the plot window to show 1 row of 4 plots

####ROC Curve
logistic_ROC_prediction_test <- prediction(logistic_probabilities_test, bkr_testing$BK)
logistic_ROC_test <- performance(logistic_ROC_prediction_test,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC_test) #Plot ROC curve

####AUC (area under curve)
auc.tmp_test <- performance(logistic_ROC_prediction_test,"auc") #Create AUC data
logistic_auc_test <- as.numeric(auc.tmp_test@y.values) #Calculate AUC
logistic_auc_test #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value


view(cbind(UnSampled_conf$table, Sampled_conf$table))
#---------------------XGBOOST: 


train.control_xgb <- trainControl(method = "cv", number = 5, search='random')
xgb_model <- train(BK ~., data = bkr_training_smpl, method = "xgbTree",
                   tuneLength = 30, trControl = train.control_xgb)
print(xgb_model)

xgb_tuned  <- xgboost(data = data.matrix(training.x[,-1]), 
                      eta = xgb_model$results$eta[which.min(xgb_model$results$RMSE)], 
                      max_depth = xgb_model$results$max_depth[which.min(xgb_model$results$RMSE)], 
                      nround=xgb_model$results$nrounds[which.min(xgb_model$results$RMSE)], 
                      gamma=xgb_model$results$gamma[which.min(xgb_model$results$RMSE)],
                      colsample_bytree=xgb_model$results$colsample_bytree[which.min(xgb_model$results$RMSE)],
                      min_child_weight=xgb_model$results$min_child_weight[which.min(xgb_model$results$RMSE)],
                      subsample=xgb_model$results$subsample[which.min(xgb_model$results$RMSE)])

xgb_tuned_prediction <- predict(xgb_tuned,newdata=testing.x[,-1]) 
postResample(pred = xgb_tuned_prediction, obs = testing$`Output.Return..`)
