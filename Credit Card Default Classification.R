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

setwd("~/Desktop/MMA/MMA867 Predictive Modelling/Assignment 3")
credit_data <- read_xlsx("credit data.xlsx")
applicant_data <- read_xlsx("new applications.xlsx")

applicant_data$default_0 <- 0
credit_data$dataset_fl <- 'train' 
applicant_data$dataset_fl <- 'test' 
for (i in 1:1000){applicant_data$ID[i] <- 24000+i}
applicant_data$ID <- as.numeric(applicant_data$ID)
final_credit_data <- rbind(credit_data, applicant_data)

head(final_credit_data)
tail(final_credit_data)
str(final_credit_data)
sum(is.na(final_credit_data))
summary(final_credit_data)
dim(final_credit_data)

table(final_credit_data$AGE)
table(final_credit_data$SEX)
table(final_credit_data$EDUCATION)
table(final_credit_data$MARRIAGE)
table(final_credit_data$PAY_1)
table(final_credit_data$PAY_2)
table(final_credit_data$PAY_3)
table(final_credit_data$PAY_4)
table(final_credit_data$PAY_5)
table(final_credit_data$PAY_6)
table(final_credit_data$default_0)

final_credit_data$MARRIAGE <- ifelse(final_credit_data$MARRIAGE == 0, 3, final_credit_data$MARRIAGE)
final_credit_data$EDUCATION <- ifelse(final_credit_data$EDUCATION %in% c(0,5,6), 4, final_credit_data$EDUCATION)

#Balance limits by gender and education
d1 <- ggplot(final_credit_data, aes(as.factor(SEX), (LIMIT_BAL/1000), fill=as.factor(EDUCATION))) + 
  geom_boxplot() +
  xlab("SEX") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Accent")

# Balance limits by education and gender
d2 <- ggplot(final_credit_data, aes(as.factor(EDUCATION), (LIMIT_BAL/1000), fill=as.factor(SEX))) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")

# Balance limits by workstate and education
d3 <-ggplot(final_credit_data, aes(as.factor(EDUCATION), (LIMIT_BAL/1000), fill=as.factor(MARRIAGE))) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") 

grid.arrange(d1, d2, d3)

qplot(as.factor(EDUCATION), LIMIT_BAL, data=final_credit_data, geom=c("boxplot", "jitter"), 
      fill=EDUCATION, main="LIMIT_AMT by Education cat",
      xlab="", ylab="amount of given credit")

#Feature Engineering

final_credit_data$WORKING <- ""
final_credit_data$MARRIAGE_SEX <- ""
final_credit_data$SEX_AGE <- ""
final_credit_data$CREDIT_LIMIT_BINS <- ""
final_credit_data$AGEBINS <- cut(final_credit_data$AGE,6,labels = c(1,2,3,4,5,6))
final_credit_data$AGEBINS <- ifelse(final_credit_data$AGEBINS == 6, 5, final_credit_data$AGEBINS)

for (i in 1:nrow(final_credit_data)) {
  if ((final_credit_data[i,7] + final_credit_data[i,8] +final_credit_data[i,9]+final_credit_data[i,10] +final_credit_data[i,11]+final_credit_data[i,12]) <= 0){
    final_credit_data[i,27] <- "Yes"  
  }
  else {
    final_credit_data[i,27] <- "No"         
  }
}

final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 0 & final_credit_data$LIMIT_BAL <= 5000), 1, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 5001 & final_credit_data$LIMIT_BAL <= 9999), 2, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 10000 & final_credit_data$LIMIT_BAL <= 14999), 3, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 15000 & final_credit_data$LIMIT_BAL <= 19999), 4, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 20000 & final_credit_data$LIMIT_BAL <= 24999), 5, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 25000 & final_credit_data$LIMIT_BAL <= 39999), 6, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 40000 & final_credit_data$LIMIT_BAL <= 49999), 7, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 50000 & final_credit_data$LIMIT_BAL <= 74999), 8, final_credit_data$CREDIT_LIMIT_BINS)
final_credit_data$CREDIT_LIMIT_BINS <- ifelse((final_credit_data$LIMIT_BAL >= 75000), 9, final_credit_data$CREDIT_LIMIT_BINS)

final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 1 & final_credit_data$MARRIAGE == 1), 1, final_credit_data$MARRIAGE_SEX)
final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 1 & final_credit_data$MARRIAGE == 2), 2, final_credit_data$MARRIAGE_SEX)
final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 1 & final_credit_data$MARRIAGE == 3), 3, final_credit_data$MARRIAGE_SEX)
final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 2 & final_credit_data$MARRIAGE == 1), 4, final_credit_data$MARRIAGE_SEX)
final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 2 & final_credit_data$MARRIAGE == 2), 5, final_credit_data$MARRIAGE_SEX)
final_credit_data$MARRIAGE_SEX <- ifelse((final_credit_data$SEX == 2 & final_credit_data$MARRIAGE == 3), 6, final_credit_data$MARRIAGE_SEX)

final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 1 & final_credit_data$SEX == 1), 1, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 2 & final_credit_data$SEX == 1), 1, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 3 & final_credit_data$SEX == 1), 1, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 4 & final_credit_data$SEX == 1), 1, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 5 & final_credit_data$SEX == 1), 1, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 1 & final_credit_data$SEX == 2), 2, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 2 & final_credit_data$SEX == 2), 2, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 3 & final_credit_data$SEX == 2), 2, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 4 & final_credit_data$SEX == 2), 2, final_credit_data$SEX_AGE)
final_credit_data$SEX_AGE <- ifelse((final_credit_data$AGEBINS == 5 & final_credit_data$SEX == 2), 2, final_credit_data$SEX_AGE)

#Client in a given month
final_credit_data$CLIENT1 <- 1
final_credit_data$CLIENT2 <- 1
final_credit_data$CLIENT3 <- 1
final_credit_data$CLIENT4 <- 1
final_credit_data$CLIENT5 <- 1
final_credit_data$CLIENT6 <- 1

final_credit_data$CLIENT1 <- ifelse(final_credit_data$PAY_1 == 0 | final_credit_data$BILL_AMT1 == 0 | final_credit_data$PAY_AMT1 == 0, 0, final_credit_data$CLIENT1)
final_credit_data$CLIENT2 <- ifelse(final_credit_data$PAY_2 == 0 | final_credit_data$BILL_AMT2 == 0 | final_credit_data$PAY_AMT2 == 0, 0, final_credit_data$CLIENT2)
final_credit_data$CLIENT3 <- ifelse(final_credit_data$PAY_3 == 0 | final_credit_data$BILL_AMT3 == 0 | final_credit_data$PAY_AMT3 == 0, 0, final_credit_data$CLIENT3)
final_credit_data$CLIENT4 <- ifelse(final_credit_data$PAY_4 == 0 | final_credit_data$BILL_AMT4 == 0 | final_credit_data$PAY_AMT4 == 0, 0, final_credit_data$CLIENT4)
final_credit_data$CLIENT5 <- ifelse(final_credit_data$PAY_5 == 0 | final_credit_data$BILL_AMT5 == 0 | final_credit_data$PAY_AMT5 == 0, 0, final_credit_data$CLIENT5)
final_credit_data$CLIENT6 <- ifelse(final_credit_data$PAY_6 == 0 | final_credit_data$BILL_AMT6 == 0 | final_credit_data$PAY_AMT6 == 0, 0, final_credit_data$CLIENT6)

#feature to describe client expense behaviour
final_credit_data$AVG_EXP5 <- (final_credit_data$BILL_AMT5 - (final_credit_data$BILL_AMT6 - final_credit_data$PAY_AMT5)) / final_credit_data$LIMIT_BAL
final_credit_data$AVG_EXP4 <- (((final_credit_data$BILL_AMT5 - (final_credit_data$BILL_AMT6 - final_credit_data$PAY_AMT5)) + (final_credit_data$BILL_AMT4 - (final_credit_data$BILL_AMT5 - final_credit_data$PAY_AMT4))) /2 ) / final_credit_data$LIMIT_BAL
final_credit_data$AVG_EXP3 <- (((final_credit_data$BILL_AMT5 - (final_credit_data$BILL_AMT6 - final_credit_data$PAY_AMT5)) + (final_credit_data$BILL_AMT4 - (final_credit_data$BILL_AMT5 - final_credit_data$PAY_AMT4)) + (final_credit_data$BILL_AMT3 - (final_credit_data$BILL_AMT4 - final_credit_data$PAY_AMT3))) / 3) / final_credit_data$LIMIT_BAL
final_credit_data$AVG_EXP2 <- (( (final_credit_data$BILL_AMT5 - (final_credit_data$BILL_AMT6 - final_credit_data$PAY_AMT5)) + (final_credit_data$BILL_AMT4 - (final_credit_data$BILL_AMT5 - final_credit_data$PAY_AMT4)) + (final_credit_data$BILL_AMT3 - (final_credit_data$BILL_AMT4 - final_credit_data$PAY_AMT3)) + (final_credit_data$BILL_AMT2 - (final_credit_data$BILL_AMT3 - final_credit_data$PAY_AMT2)) ) / 4 ) / final_credit_data$LIMIT_BAL
final_credit_data$AVG_EXP1 <- (((final_credit_data$BILL_AMT5 - (final_credit_data$BILL_AMT6 - final_credit_data$PAY_AMT5)) + (final_credit_data$BILL_AMT4 - (final_credit_data$BILL_AMT5 - final_credit_data$PAY_AMT4)) + (final_credit_data$BILL_AMT3 - (final_credit_data$BILL_AMT4 - final_credit_data$PAY_AMT3)) + (final_credit_data$BILL_AMT2 - (final_credit_data$BILL_AMT3 - final_credit_data$PAY_AMT2)) + (final_credit_data$BILL_AMT1 - (final_credit_data$BILL_AMT2 - final_credit_data$PAY_AMT1))) / 5 )/ final_credit_data$LIMIT_BAL

#difference between Limilt and Bill amount
final_credit_data$DIFF_LIMIT_BILL1 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT1) / final_credit_data$LIMIT_BAL
final_credit_data$DIFF_LIMIT_BILL2 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT2) / final_credit_data$LIMIT_BAL
final_credit_data$DIFF_LIMIT_BILL3 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT3) / final_credit_data$LIMIT_BAL
final_credit_data$DIFF_LIMIT_BILL4 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT4) / final_credit_data$LIMIT_BAL
final_credit_data$DIFF_LIMIT_BILL5 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT5) / final_credit_data$LIMIT_BAL
final_credit_data$DIFF_LIMIT_BILL6 <- (final_credit_data$LIMIT_BAL - final_credit_data$BILL_AMT6) / final_credit_data$LIMIT_BAL

final_credit_data$AGE <- ifelse(final_credit_data$AGE %in% c(79,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57), "Age_Other", final_credit_data$AGE)

final_credit_data$SEX <- as.factor(final_credit_data$SEX)
final_credit_data$EDUCATION <- as.factor(final_credit_data$EDUCATION)
final_credit_data$MARRIAGE <- as.factor(final_credit_data$MARRIAGE)
final_credit_data$default_0 <- as.factor(final_credit_data$default_0)
final_credit_data$WORKING <- as.factor(final_credit_data$WORKING)
final_credit_data$AGEBINS <- as.factor(final_credit_data$AGEBINS)
final_credit_data$PAY_1 <- as.factor(final_credit_data$PAY_1)
final_credit_data$PAY_2 <- as.factor(final_credit_data$PAY_2)
final_credit_data$PAY_3 <- as.factor(final_credit_data$PAY_3)
final_credit_data$PAY_4 <- as.factor(final_credit_data$PAY_4)
final_credit_data$PAY_5 <- as.factor(final_credit_data$PAY_5)
final_credit_data$PAY_6 <- as.factor(final_credit_data$PAY_6)
final_credit_data$MARRIAGE_SEX <- as.factor(final_credit_data$MARRIAGE_SEX)
final_credit_data$SEX_AGE <- as.factor(final_credit_data$SEX_AGE)
final_credit_data$CLIENT1 <- as.factor(final_credit_data$CLIENT1)
final_credit_data$CLIENT2 <- as.factor(final_credit_data$CLIENT2)
final_credit_data$CLIENT3 <- as.factor(final_credit_data$CLIENT3)
final_credit_data$CLIENT4 <- as.factor(final_credit_data$CLIENT4)
final_credit_data$CLIENT5 <- as.factor(final_credit_data$CLIENT5)
final_credit_data$CLIENT6 <- as.factor(final_credit_data$CLIENT6)
final_credit_data$AGE <- as.factor(final_credit_data$AGE)
final_credit_data$CREDIT_LIMIT_BINS <- as.factor(final_credit_data$CREDIT_LIMIT_BINS)

levels(final_credit_data$SEX) <- c("Male","Female")
levels(final_credit_data$EDUCATION) <- c("Graduate", "Undergraduate", "High School", "Other")
levels(final_credit_data$MARRIAGE ) <- c("Married" , "Single" ,"Other")
#levels(final_credit_data$default_0) <- c("No" , "Yes")

clean_train_data <- subset(final_credit_data, dataset_fl == 'train')
clean_predict_data <- subset(final_credit_data, dataset_fl == 'test')

clean_train_data <- subset(clean_train_data, select = -c(ID, dataset_fl,AGE,SEX_AGE))
clean_predict_data <- subset(clean_predict_data, select = -c(ID, dataset_fl,AGE,SEX_AGE))

set.seed(77850) 
inTrain <- createDataPartition(y = clean_train_data$default_0,
                               p = 19999/24000, list = FALSE)
training <- clean_train_data[ inTrain,]
testing <- clean_train_data[ -inTrain,]

#Logistic Model

model_logistic <- glm(default_0 ~ LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                        BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                        CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                        DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6
                      , data=training, family="binomial"(link="logit"))
summary(model_logistic) 

logistic_probabilities <- predict(model_logistic,newdata=testing,type="response") 
logistic_classification <- rep("1",4000)
logistic_classification[logistic_probabilities < 0.250] = "0" 
#logistic_classification<-as.factor(logistic_classification)

logistic_testing_results <- cbind(logistic_probabilities, logistic_classification)
write.csv(logistic_testing_results, "logistic_testing_model_prob_gender_025.csv", row.names = TRUE)

confusionMatrix(table(logistic_classification, testing$default_0,positive = "1"))

logistic_ROC_testing <- prediction(logistic_probabilities, testing$default_0)

logistic_ROC <- performance(logistic_ROC_testing,"tpr","fpr")
plot(logistic_ROC) #Plot ROC curve
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) 

logistic_prediction <- predict(model_logistic,newdata=clean_predict_data,type="response") 

write.csv(logistic_prediction, "logistic_model_prediction_nogender.csv",row.names = TRUE)
logistic_ROC_testing <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_testing,"tpr","fpr")
plot(logistic_ROC) #Plot ROC curve
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) 

auc.tmp <- performance(logistic_ROC_testing,"auc") 
logistic_auc_testing <- as.numeric(auc.tmp@y.values)
logistic_auc_testing 

#Stepwise

model_logistic_stepwiseAIC <- stepAIC(model_logistic,direction = c("both"),trace = 1) 
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) 
par(mfrow=c(1,1))

step_logistic_probabilities <- predict(model_logistic_stepwiseAIC,newdata=testing,type="response")
step_logistic_classification <- rep("1",4000)
step_logistic_classification[step_logistic_probabilities < 0.250] = "0" 
#step_logistic_classification <- as.factor(step_logistic_classification)

step_logistic_testing_results <- cbind(step_logistic_probabilities, step_logistic_classification)
write.csv(step_logistic_testing_results, "stepwise_testing_model_prob_gender_025.csv",row.names = TRUE)

# CTree

ctree_tree<-ctree(default_0 ~ LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                    BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                    CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                    DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6,data=training) 
plot(ctree_tree, gp = gpar(fontsize = 6))

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") 
ctree_classification<-rep("1",4000)
ctree_classification[ctree_probabilities[,2] < 0.250]="0" 
#ctree_classification<-as.factor(ctree_classification)

ctree_testing_results <- cbind(ctree_probabilities, ctree_classification)
write.csv(ctree_testing_results, "ctree_testing_prob_gender_025.csv",row.names = TRUE)

# RPart

CART_cp = rpart.control(cp = 0.0005)
rpart_tree<-rpart(default_0 ~ LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                    BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                    CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                    DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6,
                  data=training, method="class", control=CART_cp)

printcp(rpart_tree)
plotcp(rpart_tree)

prunned_rpart_tree<-prune(rpart_tree, cp=0.0005)
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7))
rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") 
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0)
#rpart_prediction_class <- predict(prunned_rpart_tree,newdata=testing, type="class")
rpart_classification <- rep("1",4000)
rpart_classification[rpart_probabilities_testing[,2] < 0.250] = "0" 
#rpart_classification <- as.factor(rpart_classification)

rpart_testing_results <- cbind(rpart_probabilities_testing, rpart_classification)
write.csv(rpart_testing_results, "rpart_testing_prob_gender_025.csv",row.names = TRUE)

# XGBoost
training.x <- model.matrix(default_0 ~
LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
  BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
  CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
  DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6, data = training)
testing.x <-  model.matrix(default_0 ~ LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                             BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                             CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                             DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6, data = testing)
predict.x <-  model.matrix(default_0 ~ LIMIT_BAL+CREDIT_LIMIT_BINS+SEX+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                             BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                             CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                             DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6, data = clean_predict_data)

model_XGboost  <- xgboost(data = data.matrix(training.x[,-1]), 
                          label = as.numeric(as.character(training$default_0)), 
                          eta = 0.1,
                          max_depth = 20, 
                          nround=50, 
                          objective = "binary:logistic")

XGboost_testing <- predict(model_XGboost,newdata=testing.x[,-1], type="response") 
XGboost_classification <- rep("1",4000)
XGboost_classification[XGboost_testing < 0.250] = "0" 
#XGboost_classification <- as.factor(XGboost_classification)

XGboost_testing_results <- cbind(XGboost_testing, XGboost_classification)
write.csv(XGboost_testing_results, "XGboost_prediction_prob_gender_025.csv")

#Random Forest

model_forest <- randomForest(default_0 ~ LIMIT_BAL+SEX+CREDIT_LIMIT_BINS+EDUCATION+MARRIAGE+PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                               BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6+WORKING+AGEBINS+CLIENT1+
                               CLIENT2+CLIENT3+CLIENT4+CLIENT5+CLIENT6+AVG_EXP5+AVG_EXP1+AVG_EXP2+AVG_EXP3+AVG_EXP4+DIFF_LIMIT_BILL1+BILL_AMT2+
                               DIFF_LIMIT_BILL2+DIFF_LIMIT_BILL3+DIFF_LIMIT_BILL4+DIFF_LIMIT_BILL5+DIFF_LIMIT_BILL6, 
                             data=training, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.75, 0.25),type="classification") 
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

###Finding predicitons: probabilities and classification
forest_probabilities <- predict(model_forest,newdata=testing,type="prob")
forest_classification <- rep("1",4000)
forest_classification[forest_probabilities[,2] < 0.250] = "0" 
#forest_classification <- as.factor(forest_classification)

forest_testing_results <- cbind(forest_probabilities, forest_classification)
write.csv(forest_testing_results, "forest_testing_prob_gender_025.csv",row.names = TRUE)

confusionMatrix(table(forest_classification, testing$default_0,positive = "1"))

forest_ROC_testing <- prediction(forest_probabilities[,2], testing$default_0) 
forest_ROC <- performance(forest_ROC_testing,"tpr","fpr")

plot(forest_ROC, add=TRUE, col="blue") 

plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10)

AUC.tmp4 <- performance(forest_ROC_testing,"auc") 
forest_AUC <- as.numeric(AUC.tmp4@y.values) 

forest_AUC

forest_prediction <- predict(model_forest,newdata=clean_predict_data,type="prob") 

write.csv(forest_prediction, "forest_model_prediction_nogender.csv",row.names = TRUE)

#Confusion Matrix
#Logistic
#confusionMatrix(logistic_classification, testing$default_0) #Display confusion matrix
confusionMatrix(table(logistic_classification, testing$default_0,positive = "1"))
#Stepwise
confusionMatrix(table(step_logistic_classification, testing$default_0,positive = "1"))
#CTREE
#confusionMatrix(ctree_classification, testing$default_0,positive = "1")
confusionMatrix(table(ctree_classification, testing$default_0,positive = "1"))
#RPART
#confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1")
confusionMatrix(table(rpart_classification, testing$default_0,positive = "1"))
#Random Forest
confusionMatrix(table(forest_classification, testing$default_0,positive = "1"))
#XGboost
confusionMatrix(as.factor(ifelse(XGboost_testing < 0.250,0,1)),testing$default_0,positive="1") 
#NN
#confusionMatrix(NN_testing,testing$default_0,positive = "1")

# ROC Curve
logistic_ROC_testing <- prediction(logistic_probabilities, testing$default_0)
step_logistic_ROC_testing <- prediction(step_logistic_probabilities, testing$default_0)
ctree_probabilities_testing <- predict(ctree_tree,newdata=testing,type = "prob") 
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0)
rpart_probabilities_testing <- predict(prunned_rpart_tree,newdata=testing,type = "prob") 
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0)
forest_ROC_testing <- prediction(forest_probabilities[,2], testing$default_0) 
XGboost_pred_testing <- prediction(XGboost_testing, testing$default_0) 
#NN_probabilities_testing <- predict(model_NN,newdata=testing,type = "prob") 
#NN_pred_testing <- prediction(NN_probabilities_testing[,2], testing$default_0) 

logistic_ROC <- performance(logistic_ROC_testing,"tpr","fpr")
step_ROC <- performance(step_logistic_ROC_testing,"tpr","fpr")
ctree_ROC <- performance(ctree_pred_testing,"tpr","fpr") 
rpart_ROC <- performance(rpart_pred_testing,"tpr","fpr")
forest_ROC <- performance(forest_ROC_testing,"tpr","fpr")
XGboost_ROC <- performance(XGboost_pred_testing,"tpr","fpr")
#NN_ROC <- performance(NN_pred_testing,"tpr","fpr") 

#Plot
plot(logistic_ROC) #Plot ROC curve
plot(step_ROC, add=TRUE, col="red") 
plot(ctree_ROC, add=TRUE, col="darkblue")
plot(rpart_ROC, add=TRUE, col="purple")
plot(forest_ROC, add=TRUE, col="blue") 
plot(XGboost_ROC, add=TRUE, col="green") 
#plot(NN_ROC,add=TRUE, col="purple")
legend("right", legend=c("Logistic","Stepwise","CTREE","RPART","Randomforest","XGboost"), col=c("black","red","darkblue","purple","blue","green"), lty=1:2, cex=0.8)

# Lift Chart

plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) 
plotLift(step_logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10)
plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10)
plotLift(rpart_probabilities_testing[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10)
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10)
plotLift(XGboost_testing, testing$default_0, cumulative = TRUE, n.buckets = 10) 
#plotLift(NN_testing,  testing$default_0, cumulative = TRUE, n.buckets = 10)

# AUC

auc.tmp <- performance(logistic_ROC_testing,"auc") 
logistic_auc_testing <- as.numeric(auc.tmp@y.values) 

auc.tmp1 <- performance(step_logistic_ROC_testing,"auc") 
step_logistic_auc_testing <- as.numeric(auc.tmp1@y.values) 

auc.tmp2 <- performance(ctree_pred_testing,"auc") 
ctree_auc_testing <- as.numeric(auc.tmp2@y.values)

auc.tmp3 <- performance(rpart_pred_testing,"auc") 
rpart_auc_testing <- as.numeric(auc.tmp3@y.values) 

AUC.tmp4 <- performance(forest_ROC_testing,"auc") 
forest_AUC <- as.numeric(AUC.tmp4@y.values) 

auc.tmp5 <- performance(XGboost_pred_testing,"auc")
XGboost_auc_testing <- as.numeric(auc.tmp5@y.values) 

#auc.tmp6 <- performance(NN_pred_testing,"auc") 
#NN_auc_testing <- as.numeric(auc.tmp6@y.values) 

logistic_auc_testing 
step_logistic_auc_testing
ctree_auc_testing
rpart_auc_testing
forest_AUC
XGboost_auc_testing 
#NN_auc_testing

# Write Prediction

logistic_prediction <- predict(model_logistic,newdata=clean_predict_data,type="response") 
step_logistic_prediction <- predict(model_logistic_stepwiseAIC,newdata=clean_predict_data,type="response") 
ctree_prediction <- predict(ctree_tree,newdata=clean_predict_data,type="prob") 
rpart_prediction <- predict(prunned_rpart_tree,newdata=clean_predict_data,type = "prob")  
forest_prediction <- predict(model_forest,newdata=clean_predict_data,type="prob") 
XGboost_prediction <- predict(model_XGboost,newdata=predict.x[,-1], type="response") 
#NN_prediction <- predict(model_NN, newdata=clean_predict_data)

write.csv(logistic_prediction, "logistic_model_prediction_gender.csv",row.names = TRUE)
write.csv(step_logistic_prediction, "stepwise_model_prediction_gender.csv",row.names = TRUE)
write.csv(ctree_prediction, "ctree_model_prediction_gender.csv",row.names = TRUE)
write.csv(rpart_prediction, "rpart_model_prediction_gender.csv",row.names = TRUE)
write.csv(forest_prediction, "forest_model_prediction_gender.csv",row.names = TRUE)
write.csv(XGboost_prediction, "XGboost_model_prediction_gender.csv")
#write.csv(NN_prediction, "NN_model_prediction_gender.csv",row.names = TRUE)


