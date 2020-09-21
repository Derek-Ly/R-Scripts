if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071","randomForest","xgboost","corrplot","ggthemes","devtools","PerformanceAnalytics","FactoMineR","Hmisc","tableplot") 
pacman::p_load("partykit","rpart","nnet")

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
library(tidyverse)
library(Metrics)

setwd("~/Desktop/MMA/MMA823 Analytics for Financial Markets/Assignment 1")
df <- read_excel("A1Data_MMAI.xlsx")
df$Year <- as.factor(df$Year)
df$Year <- NULL

str(df)
names(df)[5]<-paste("LeverageRatio") 
names(df)[6]<-paste("MarketBookRatio") 
names(df)[7]<-paste("TobinsQ") 
names(df)[9]<-paste("ExcessReturn")
names(df)[15]<-paste("TotalVolatility") 
names(df)[length(df)]<-paste("OutputReturn") 

id <- as.data.frame(1:26841)
names(id) <- 'id'

df <- bind_cols(id,df)

#Feature Engineering 
#Liquidity/Profitable
df$liq_prof <- ifelse(df$Liquidity/df$Profitability==Inf,0,
                      ifelse(df$Liquidity/df$Profitability==-Inf,0,df$Liquidity/df$Profitability))
df$alpha_liquidity <- df$alpha * df$Liquidity
df$alpha_profit <- df$alpha * df$Profitability
df$bmkt_liquidity <- df$b_mkt * df$Liquidity
df$bmkt_profit <- df$b_mkt * df$Profitability




#liquidity/Productivity
df$liq_prod <- ifelse(df$Liquidity/df$Productivity==Inf,0,
                      ifelse(df$Liquidity/df$Productivity==-Inf,0,df$Liquidity/df$Productivity))
df$liq_prod <- replace_na(df$liq_prod,0)
df$alpha_productivity <- df$alpha * df$Productivity
df$bmkt_productivity <- df$b_mkt * df$Productivity



#3 Profitability/Productivity
df$Prof_Prod <- ifelse(df$Profitability/df$Productivity==Inf,0,
                       ifelse(df$Profitability/df$Productivity==-Inf,0,df$Profitability/df$Productivity))

str(df)

set.seed(8589)
inTrain <- createDataPartition(y = df$OutputReturn,
                               p = 21000/26481, list = FALSE)
training <- df[ inTrain,]
testing <- df[ -inTrain,]

### LASSO

training <-subset(df, id<=21288)
testing <- subset(df, id>21288)

y<-training$OutputReturn

X<-model.matrix(id~.-OutputReturn , df)[,-1]

X<-cbind(df$id,X)

X_training<-subset(X,X[,1]<=21288)
X_testing<-subset(X,X[,1]>=21289)

#Ridge (alpha = 0)
ridge.fit<-glmnet(x = X_training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X_training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min
penalty.ridge
ridge.opt.fit <-glmnet(x = X_training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- predict(ridge.opt.fit, s = penalty.ridge, newx =X_testing)
postResample(pred = ridge.testing, obs = testing$OutputReturn)


#LASSO (alpha=1)
lasso.fit<-glmnet(x = X_training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = X_training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
penalty.lasso #see where it was on the graph
lasso.opt.fit <-glmnet(x = X_training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #sub-test training model coefficients

lasso.testing <- predict(lasso.opt.fit, s = penalty.lasso, newx =X_testing)
postResample(pred = lasso.testing, obs = testing$OutputReturn)

### randomForest basic

model_forest <- randomForest(OutputReturn~.,data=training, importance=TRUE,proximity=TRUE,type="regression") 
rf_prediction <- predict(model_forest,newdata=testing)

postResample(pred = rf_prediction, obs = testing$OutputReturn)
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

### randomForest parameter tuning, this part needs to be worked on

# train.control_rf <- trainControl(method = "cv", number =  5, search="grid")
# tunegrid_rf <- expand.grid(.mtry=c(1:15))
# rf_model <- train(OutputReturn~., data = training, method = "rf",
#                   tuneGrid=tunegrid_rf, trControl = train.control_rf)
# 
# print(rf_model)
# 
# rf_tuned <- randomForest(OutputReturn~., data=training, mtry=rf_model$results$mtry[which.min(rf_model$results$RMSE)],
#                          importance=TRUE,proximity=TRUE,type="regression")
# 
# plot(rf_tuned)
# importance(rf_tuned)
# varImpPlot(rf_tuned)
# 
# rf_tuned_prediction <- predict(rf_tuned,newdata=testing)
# 
# postResample(pred = rf_tuned_prediction, obs = testing$OutputReturn)

### XGBoost basic

training.x <- model.matrix(OutputReturn~., data = training)
testing.x <-  model.matrix(OutputReturn~., data = testing)


model_XGboost  <- xgboost(data = data.matrix(training.x[,-1]), 
                          label = training$OutputReturn, 
                          eta = 0.1,
                          max_depth = 20, 
                          nround=50,)

xgb_prediction <- predict(model_XGboost,newdata=testing.x[,-1]) 
postResample(pred = xgb_prediction, obs = testing$OutputReturn)

### XGB parameter tuning

set.seed(8589)
train.control_xgb <- trainControl(method = "cv", number = 5, search='random')
xgb_model <- train(OutputReturn ~., data = training, method = "xgbTree",
                   tuneLength = 30, trControl = train.control_xgb)
print(xgb_model)

xgb_tuned  <- xgboost(data = data.matrix(training.x[,-1]), 
                      label = training$OutputReturn,
                      eta = xgb_model$results$eta[which.min(xgb_model$results$RMSE)], 
                      max_depth = xgb_model$results$max_depth[which.min(xgb_model$results$RMSE)], 
                      nround=xgb_model$results$nrounds[which.min(xgb_model$results$RMSE)], 
                      gamma=xgb_model$results$gamma[which.min(xgb_model$results$RMSE)],
                      colsample_bytree=xgb_model$results$colsample_bytree[which.min(xgb_model$results$RMSE)],
                      min_child_weight=xgb_model$results$min_child_weight[which.min(xgb_model$results$RMSE)],
                      subsample=xgb_model$results$subsample[which.min(xgb_model$results$RMSE)])

xgb_tuned_prediction <- predict(xgb_tuned,newdata=testing.x[,-1]) 
postResample(pred = xgb_tuned_prediction, obs = testing$OutputReturn)
