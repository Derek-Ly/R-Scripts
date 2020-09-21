library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('class') # imputation with kNN
library('glmnet') #For ridge and lasso
library('corrplot') #for the correlation plot

setwd("~/Desktop/MMA/MMA867 Predictive Modelling/Assignment 1/Housing")

test <- read.csv("test.csv") 
train <- read.csv("train.csv")

housing <- bind_rows(train,test)

ncol(housing)
str(housing)
md.pattern(housing)
sort(colSums(sapply(housing[which(colSums(is.na(train)) > 0)], is.na)), decreasing = TRUE)

housing$MSSubClass <- as.factor(housing$MSSubClass)
housing$Condition2 <- as.factor(housing$Condition2)
housing$HouseStyle <- as.factor(housing$HouseStyle)
housing$Utilities <- as.factor(housing$Utilities)
housing$RoofMatl <- as.factor(housing$RoofMatl)
housing$Exterior1st <- as.factor(housing$Exterior1st)
housing$Exterior2nd <- as.factor(housing$Exterior2nd)
housing$Heating <- as.factor(housing$Heating)
housing$Electrical <- as.factor(housing$Electrical)
housing$MoSold <- as.factor(housing$MoSold)
housing$SalePrice <- as.numeric(housing$SalePrice)
housing$GrLivArea <- as.numeric(housing$GrLivArea)

# Data Cleaning

sum(is.na(housing$Alley))
sum(is.na(housing$Alley))/length(housing$Alley)
housing$Alley <- as.character(housing$Alley)
housing$Alley[is.na(housing$Alley)] <- "None"
housing$Alley <- as.factor(housing$Alley)

sum(is.na(housing$LotFrontage))
housing$LotFrontage[is.na(housing$LotFrontage)] <- 0
sum(is.na(housing$LotFrontage))

sum(is.na(housing$PoolQC))
housing$PoolQC[is.na(housing$PoolQC)] <- "None"
housing$PoolQC<- as.factor(housing$PoolQC)

sum(is.na(housing$MiscFeature))
housing$MiscFeature[is.na(housing$MiscFeature)] <- "None"
housing$MiscFeature<- as.factor(housing$MiscFeature)

sum(is.na(housing$Fence))
housing$Fence <- as.character(housing$Fence)
housing$Fence[is.na(housing$Fence)] <- "None"
housing$Fence <- as.factor(housing$Fence)

sum(is.na(housing$GarageQual))
housing$GarageQual[is.na(housing$GarageQual)] <- "None"
housing$GarageQual <- as.factor(housing$GarageQual)

sum(is.na(housing$FireplaceQu))
housing$FireplaceQu <- as.character(housing$FireplaceQu)
housing$FireplaceQu[is.na(housing$FireplaceQu)] <- "None"
housing$FireplaceQu <- as.factor(housing$FireplaceQu)

sum(is.na(housing$GarageCond))
housing$GarageCond <- as.character(housing$GarageCond)
housing$GarageCond[is.na(housing$GarageCond)] <- "None"
housing$GarageCond <- as.factor(housing$GarageCond)

sum(is.na(housing$GarageFinish))
housing$GarageFinish <- as.character(housing$GarageFinish)
housing$GarageFinish[is.na(housing$GarageFinish)] <- "None"
housing$GarageFinish <- as.factor(housing$GarageFinish)

sum(is.na(housing$GarageType))
housing$GarageType <- as.character(housing$GarageType)
housing$GarageType[is.na(housing$GarageType)] <- "None"
housing$GarageType <- as.factor(housing$GarageType)

sum(is.na(housing$GarageYrBlt))
housing$GarageYrBlt <- as.character(housing$GarageYrBlt)
housing$GarageYrBlt[is.na(housing$GarageYrBlt)] <- "None"
housing$GarageYrBlt<- as.factor(housing$GarageYrBlt)

sum(is.na(housing$BsmtExposure))
housing$BsmtExposure <- as.character(housing$BsmtExposure)
housing$BsmtExposure[is.na(housing$BsmtExposure)] <- "No"
housing$BsmtExposure <- as.factor(housing$BsmtExposure)

sum(is.na(housing$BsmtCond))
housing$BsmtCond <- as.character(housing$BsmtCond)
housing$BsmtCond[is.na(housing$BsmtCond)] <- "None"
housing$BsmtCond <- as.factor(housing$BsmtCond)

sum(is.na(housing$BsmtQual))
housing$BsmtQual <- as.character(housing$BsmtQual)
housing$BsmtQual[is.na(housing$BsmtQual)] <- "None"
housing$BsmtQual <- as.factor(housing$BsmtQual)

sum(is.na(housing$BsmtFinType2))
housing$BsmtFinType2 <- as.character(housing$BsmtFinType2)
housing$BsmtFinType2[is.na(housing$BsmtFinType2)] <- "None"
housing$BsmtFinType2 <- as.factor(housing$BsmtFinType2)

sum(is.na(housing$BsmtFinType1))
housing$BsmtFinType1 <- as.character(housing$BsmtFinType1)
housing$BsmtFinType1[is.na(housing$BsmtFinType1)] <- "None"
housing$BsmtFinType1 <- as.factor(housing$BsmtFinType1)

sum(is.na(housing$MasVnrType))
housing$MasVnrType <- as.character(housing$MasVnrType)
housing$MasVnrType[is.na(housing$MasVnrType)] <- "None"
housing$MasVnrType <- as.factor(housing$MasVnrType)

sum(is.na(housing$MSZoning))
summary(housing$MSZoning)
housing$MSZoning <- as.character(housing$MSZoning)
housing$MSZoning[is.na(housing$MSZoning)] <- "RL"
housing$MSZoning <- as.factor(housing$MSZoning)
sum(is.na(housing$MSZoning))

sum(is.na(housing$MasVnrArea))
sum(is.na(housing$MasVnrArea))/length(housing$MasVnrArea)
housing$MasVnrType[is.na(housing$MasVnrArea)]
housing$MasVnrArea[is.na(housing$MasVnrArea)] <- 0
str(housing$MasVnrArea) #This is a numeric now

sum(is.na(housing$Functional)) #2 NAs
summary(housing$Functional)
str(housing$Functional)
housing$Id[is.na(housing$Functional)]
housing$Functional <- as.character(housing$Functional)
housing$Functional[c(2217,2474)] <-'Typ'
sum(is.na(housing$Functional))
housing$Functional <- as.factor(housing$Functional)

sum(is.na(housing$BsmntUnfSF))

sum(is.na(housing$Exterior1st)) # 1 NA
summary(housing$Exterior1st)
str(housing$Exterior1st)
housing$Id[is.na(housing$Exterior1st)]
housing$Exterior1st <- as.character(housing$Exterior1st)
housing$Exterior1st[2152] <-'VinylSd'
sum(is.na(housing$Exterior1st))
housing$Exterior1st <- as.factor(housing$Exterior1st)

sum(is.na(housing$Exterior2nd)) # 1 NA
summary(housing$Exterior2nd)
str(housing$Exterior2nd)
housing$Id[is.na(housing$Exterior2nd)]
housing$Exterior2nd <- as.character(housing$Exterior2nd)
housing$Exterior2nd[2152] <-'VinylSd'
sum(is.na(housing$Exterior2nd))
housing$Exterior2nd <- as.factor(housing$Exterior2nd)

sum(is.na(housing$TotalBsmntSF)) 

sum(is.na(housing$GarageArea)) # 1 NA
summary(housing$GarageArea)
str(housing$GarageArea)
housing$Id[is.na(housing$GarageArea)]
housing$GarageType[2577]
housing$GarageArea[2577] <- 472.9 #Replace missing Garage Area with average garage area
sum(is.na(housing$GarageArea))

sum(is.na(housing$GarageCars)) # 1 NA
summary(housing$GarageCars)
str(housing$GarageCars)
housing$Id[is.na(housing$GarageCars)]
housing$GarageType[2577]
housing$GarageCars[2577] <- 2 #Replace missing Garage Cars with median garagecars
sum(is.na(housing$GarageArea))

sum(is.na(housing$BsmtFinSF2))
summary(housing$BsmtFinSF2)
str(housing$BsmtFinSF2)
housing$Id[is.na(housing$BsmtFinSF2)]
housing$BsmtFinType2[is.na(housing$BsmtFinSF2)]
housing$BsmtFinSF2[2121] <- 0
sum(is.na(housing$BsmtFinSF2))
str(housing$BsmtFinSF2)

sum(is.na(housing$BsmtFinSF1))
summary(housing$BsmtFinSF1)
str(housing$BsmtFinSF1)
housing$Id[is.na(housing$BsmtFinSF1)]
housing$BsmtFinType1[is.na(housing$BsmtFinSF1)]
housing$BsmtFinSF1[2121] <- 0
sum(is.na(housing$BsmtFinSF1))
str(housing$BsmtFinSF1)

sum(is.na(housing$BsmtUnfSF))
summary(housing$BsmtUnfSF)
str(housing$BsmtUnfSF)
housing$Id[is.na(housing$BsmtUnfSF)]
housing$BsmtFinType2[is.na(housing$BsmtUnfSF)]
housing$BsmtUnfSF[2121] <- 0
sum(is.na(housing$BsmtUnfSF))
str(housing$BsmtUnfSF)

sum(is.na(housing$TotalBsmtSF))
summary(housing$TotalBsmtSF)
str(housing$TotalBsmtSF)
housing$Id[is.na(housing$TotalBsmtSF)]
housing$BsmtFinType1[is.na(housing$TotalBsmtSF)]
housing$TotalBsmtSF[2121] <- 0
sum(is.na(housing$TotalBsmtSF))
str(housing$TotalBsmtSF)

sum(is.na(housing$BsmtFullBath))
summary(housing$BsmtFullBath)
str(housing$BsmtFullBath)
housing$Id[is.na(housing$BsmtFullBath)]
housing$BsmtFinType1[is.na(housing$BsmtFullBath)]
housing$BsmtFullBath[c(2121,2189)] <- 0
sum(is.na(housing$BsmtFullBath))
str(housing$BsmtFullBath)

sum(is.na(housing$BsmtHalfBath))
summary(housing$BsmtHalfBath)
str(housing$BsmtHalfBath)
housing$Id[is.na(housing$BsmtHalfBath)]
housing$BsmtFinType1[is.na(housing$BsmtHalfBath)]
housing$BsmtHalfBath[c(2121,2189)] <- 0
sum(is.na(housing$BsmtHalfBath))
str(housing$BsmtHalfBath)

sum(is.na(housing$KitchenQual)) # 1 NA
summary(housing$KitchenQual)
str(housing$KitchenQual)
housing$Id[is.na(housing$KitchenQual)]
housing$KitchenQual <- as.character(housing$KitchenQual)
housing$KitchenQual[1556] <-'TA'
sum(is.na(housing$KitchenQual))
housing$KitchenQual <- as.factor(housing$KitchenQual)

sum(is.na(housing$SaleType)) # 1 NA
summary(housing$SaleType)
str(housing$SaleType)
housing$Id[is.na(housing$SaleType)]
housing$SaleType <- as.character(housing$SaleType)
housing$SaleType[2490] <-'WD'
sum(is.na(housing$SaleType))
housing$SaleType <- as.factor(housing$SaleType)

sum(is.na(housing$BsmntFinType1))

housing$Id[is.na(housing$Electrical)]
housing$Electrical <- as.character(housing$Electrical)
housing$Electrical[1380] <-'SBrkr'
sum(is.na(housing$Electrical))
housing$Electrical <- as.factor(housing$Electrical)
str(housing$Electrical)

# Data Exploration

train <- subset(housing, Id<=1460)
test <- subset(housing, Id>1460)

numericVars <- which(sapply(train, is.numeric)) #selects all numeric variables
numericVarNames <- names(numericVars) #saves the names of each numeric variable
numVar <- train[, numericVars]
cor_numVar <- cor(numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5))) #only correlations above 0.5
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot(cor_numVar, method="number", type ="upper",tl.col="black", tl.pos = "lt")

md.pattern(train)
sum(is.na(train))

hist(housing$GrLivArea)
hist(log(housing$GrLivArea))

hist(train$SalePrice)
hist(log(train$SalePrice))

str(train)

par(mfrow=c(2,2))
plot(lm(train$SalePrice~train$GrLivArea))
1
2

cor(train$SalePrice,train$GrLivArea)

outliers <- c(524,1299)
housing <- housing[-outliers,]

train <- subset(housing, Id<=1460)
test <- subset(housing, Id>1460)

cor(train$SalePrice,train$GrLivArea)

# Feature Engineering

min(housing$YearBuilt)
max(housing$YearBuilt)
housing$Age <- housing$YrSold-housing$YearBuilt 
housing$YrSold <- as.factor(housing$YrSold)

housing$TotalArea<- housing$GrLivArea+housing$TotalBsmtSF
str(housing$TotalArea)

train <- subset(housing, Id<=1460)
test <- subset(housing, Id>1460)
cor(train$SalePrice,train$TotalArea)

par(mfrow=c(1,1))
md.pattern(test)
missingdata_test <- which(colSums(is.na(test)) > 0)
sort(colSums(sapply(train[missingdata_test], is.na)), decreasing = TRUE)

# Model Building

train <-subset(housing, Id<=1460)
test <- subset(housing, Id>1460)
sub_train<-subset(train, Id<=1200) #redefine the training data
sub_test<-subset(train, (Id>=1201 & Id<=1460))

y<-log(sub_train$SalePrice)

X<-model.matrix(Id~ TotalArea*MSSubClass + TotalArea*MSZoning+ TotalArea+ MSZoning+MSSubClass + 
                    LotFrontage + log(LotArea) + Street + Alley + LotShape +LandContour+ 
                    LotConfig + LandSlope + Neighborhood + Condition1 +
                    Condition2 + BldgType + HouseStyle + OverallQual + OverallCond +
                    Age + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd +
                    MasVnrType  + MasVnrArea+ ExterQual+ ExterCond + Foundation + BsmtQual + BsmtCond +
                    BsmtExposure + BsmtFinType1  + BsmtFinSF1 +  BsmtFinType2 + BsmtFinSF2 + TotalBsmtSF +
                    Heating+ HeatingQC + CentralAir + Electrical +
                    X1stFlrSF + X2ndFlrSF + LowQualFinSF + log(GrLivArea) + BsmtFullBath  + BsmtHalfBath+
                    FullBath + HalfBath+BedroomAbvGr +
                    KitchenAbvGr  + KitchenQual +TotRmsAbvGrd  + Fireplaces + FireplaceQu + GarageType + 
                    GarageFinish + GarageCars + GarageArea+
                    X3SsnPorch + PoolArea + MiscFeature + MiscVal + MoSold + YrSold +
                    GarageQual + GarageCond + PavedDrive + WoodDeckSF +
                    OpenPorchSF + EnclosedPorch + ScreenPorch + PoolQC + Fence +
                    SaleCondition, housing)[,-1]

X<-cbind(housing$Id,X)

X_training<-subset(X,X[,1]<=1200)
X_testing<-subset(X, (X[,1]>=1201 & X[,1]<=1460))
X_prediction<-subset(X,X[,1]>=1461)

# #Ridge (alpha = 0)
# ridge.fit<-glmnet(x = X_training, y = y, alpha = 0)
# plot(ridge.fit, xvar = "lambda")
# 
# #selecting the best penalty lambda
# crossval <-  cv.glmnet(x = X_training, y = y, alpha = 0)
# plot(crossval)
# penalty.ridge <- crossval$lambda.min 
# log(penalty.ridge) 
# ridge.opt.fit <-glmnet(x = X_training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
# coef(ridge.opt.fit)
# 
# ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X_testing))
# mean(abs(ridge.testing-sub_test$SalePrice)/sub_test$SalePrice*100) 
# 
# predicted.prices.ridge <- exp((predict(ridge.opt.fit, s = penalty.ridge, newx =X_prediction)))
# write.csv(predicted.prices.ridge, file = "Predicted Sale Prices LOG RIDGE.csv")

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X_training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = X_training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = X_training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #sub-test training model coefficients

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X_testing))
mean(abs(lasso.testing-sub_test$SalePrice)/sub_test$SalePrice*100)

# Now that I've tested the MAPE, time to use the full train dataset to make the model

y<-log(train$SalePrice)
X_training<-subset(X,X[,1]<=1460)
X_prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X_training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = X_training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = X_training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

predicted.prices.lasso <- exp((predict(lasso.opt.fit, s = penalty.lasso, newx =X_prediction)))
write.csv(predicted.prices.lasso, file = "Predicted Sale Prices LOG LASSO.csv") 