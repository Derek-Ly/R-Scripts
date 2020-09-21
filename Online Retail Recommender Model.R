library(data.table) # To create the binary matrix
library(tidyverse) #dplyr is in this        
library(knitr) #evaluate function
library(recommenderlab) #recommend function, model revovles around this
library(readxl) 
library(base) #using the paste() function to concatenate

setwd("~/Desktop/MMA/MMA831 Marketing Analysis/Assignment 2")
# setwd("~/MMA831/DOS 2")
retail <- read_excel("Online Retail.xlsx")

# Combine Invoice No and Description, this will identify the duplicates
retail$com_invoice_desc <- paste(retail$InvoiceNo, sep = ' ', retail$Description)

# Remove the duplicates, seems like if you buy certain items that don't tally up the quantity, they're saved as duplicates. 

retail <- retail[!duplicated(retail$com_invoice_desc), ]
retail$com_invoice_desc <- NULL

summary(retail)

##check NAs and remove rows with missing values
retail %>% 
  map(., ~sum(is.na(.)))

retail <- retail[complete.cases(retail), ]

#separate date and time
retail$InvoiceTime = format(retail$InvoiceDate,"%H")
retail$InvoiceMonth=format(retail$InvoiceDate,"%m")
retail$InvoiceDate<-as.Date(retail$InvoiceDate)

#adding variables 
retail$amount<-retail$Quantity*retail$UnitPrice
retail$sku<-substr(retail$StockCode,1,3)

retail$InvoiceNo<-as.factor(retail$InvoiceNo)
retail$StockCode<-as.factor(retail$StockCode)
retail$Description<-as.factor(retail$Description)
retail$Country<-as.factor(retail$Country)
retail$InvoiceMonth<-as.numeric(retail$InvoiceMonth)
retail$Quantity<-as.integer(retail$Quantity)
retail$CustomerID<-as.integer(retail$CustomerID)

#removing some the nagative with amount and set a limit
boxplot(retail$amount)
retail<-subset(retail,retail$amount>=0 & retail$amount<=10000)

# We want a binary ratings matrix, where we determine if a user had bought a certain
# item or not. The code steph thomas gave us was for real user rating, not binary
bin_rate_matrix <- retail %>% 
  select(InvoiceNo, Description) %>% 
  mutate(value = 1) %>%
  spread(Description, value, fill = 0) %>% 
  select(-InvoiceNo) %>% 
  as.matrix() %>%
  as("binaryRatingMatrix")

# These are the algorithms we will be running, along with their rules.

algo_rules<-list(
  ASSOCIATION = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
  POPULAR=list(name="POPULAR",param=list(normalize = "Z-score")),
  RAND=list(name="RANDOM", param = list(normalize = "Z-score")),
  UBCF_Cos=list(name="UBCF",param = list(nn = 50, method="Cosine")),
  IBCF_Jac=list(name="IBCF",param = list(k=5, method="Jaccard")))

# To evaluate our model, will be using k-fold cross validation with 5 folds, Patrick advised this k value of 5

eval <- evaluationScheme(bin_rate_matrix,method = "cross", k = 5, train  = 0.8, given= -1)

# For user based collaborative filtering, I chose cosine because its the metric best for item similarity
# For item based collaborative filtering, cosine was extremely low in the ROC curves, I explained this in the report

results <- evaluate(eval, algo_rules, type= "topNList", n = c(1, 3, 5, 7, 10))
results

plot(results, annotate = TRUE)+title(main = "ROC Curves") # ROC Curve
plot(results, "prec/rec", annotate = TRUE) + title(main="Position and Recall") # Position and Recall 

# Seems that IBCF is our best algorithm, lets test this against different k values

algo_rules_IBCF<-list(
  IBCF_Jac=list(name="IBCF",param = list(k=5, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=10, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=15, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=20, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=25, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=30, method="Jaccard")),
  IBCF_Jac=list(name="IBCF",param = list(k=35, method="Jaccard")))

results_IBCF <- evaluate(eval, algo_rules_IBCF, type= "topNList", n = c(1, 3, 5, 7, 10))
results_IBCF

plot(results_IBCF, annotate = TRUE)+title(main = "IBCF ROC Curves") # ROC Curve
plot(results_IBCF, "prec/rec", annotate = TRUE) + title(main="IBCF Position and Recall") # Position and Recall 
# Seems that a K value of 5 is our best model

# save the confusion matrix from item-based CF into a list
conf_mat_list <- results$IBCF %>% getConfusionMatrix()  %>%  as.list()


# Calculate average value for each folding round
# Select only columns needed and sorting out order 
as.data.frame(Reduce("+",conf_mat_list)/length(conf_mat_list)) %>%
  mutate(n = c(1, 3, 5, 7, 10)) %>%  
  select('n', 'precision', 'recall', 'TPR', 'FPR')  

avg_conf_matr <- function(results) {
  conf_mat_list <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",conf_mat_list) / length(conf_mat_list)) %>% 
    mutate(n = c(1, 3, 5, 7, 10)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

# Save the confusion matrix values into a dataframe for the results
# This will let us plot the data

results_conf_df <- results %>%
  map(avg_conf_matr) %>%
  enframe() %>% # convert atomic vector into a more familiar format of columns
  unnest()   # convert the list-column format into dataframe

results_conf_df

# Predicting a New User

# Learn the recommender model, this is super long
# IBCF with k = 5 was our best model

rec_model <- Recommender(getData(eval,'train'),method = "IBCF",param = list(k = 5))

# This is where you enter the similar items to generate the recommendation

item <- c("SAVE THE PLANET MUG",
          "WHITE METAL LANTERN",
          "SET OF 6 SOLDIER SKITTLES",
          "FIRST AID TIN")

# Create New Order Rating Matrix, this is baiscally the input to run the recommender model on and generate recommendations

new_order_rat_matrx <- retail %>% 
  select(Description) %>% 
  unique() %>% 
  mutate(value = as.numeric(Description %in% item)) %>%  
  spread(key = Description, value = value) %>%   # Spread into sparse matrix format
  as.matrix() %>%   # Change to a matrix
  as("binaryRatingMatrix")   # Convert to recommenderlab class 'binaryRatingsMatrix'

# Create the prediction based on the recommending model and the new data
pred <- predict(rec_model, newdata = new_order_rat_matrx, n=5)

# Finally, output the results
as(pred, 'list')
