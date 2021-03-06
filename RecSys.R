
library(recommenderlab)
library(tidyverse)

# Load Data

data(MovieLense)
r <- MovieLense

class(r)
str(r)


# Create a copy of the data as a dataframe, to make some exploration a bit easier.

df = as(r, "data.frame")

# Look at the first few rows.

head(df, n=20)

# Explore and Visualize

str(df)
colnames(df)
dim(df)
summary(df)

# Let's look at the rating distribution:

ggplot(df, aes(rating)) + geom_histogram(binwidth=0.5) + ggtitle("Number of each movie rating")



# Let's look at the most popular movies (with at least 10 ratings):

movie_stats <- df %>%
  group_by(item) %>%
  summarise(mean_rating = mean(rating), count=n())

movie_stats %>% filter(count > 10) %>%
  arrange(desc(mean_rating)) %>%
  head(20)

# And the least popular movies:
  
movie_stats %>% filter(count > 10) %>% arrange(mean_rating) %>% head(20)

# And the most rated movies:

movie_stats %>% arrange(desc(count)) %>% head(52)

movie_stats %>% ggplot(aes(count)) + geom_histogram(binwidth=5) + ggtitle("Number of ratings per movie")

# Let's look at users: the distribution of users' average ratings.

user_stats <- df %>% group_by(user) %>% summarise(mean_rating = mean(rating), count=n())

user_stats %>% ggplot(aes(mean_rating)) + geom_histogram(binwidth=0.2) + ggtitle("Average movie rating per user")
user_stats %>% ggplot(aes(count)) + geom_histogram(binwidth=5) + ggtitle("Number of movies rated per user")

# Let's look at a image plot or raw-ratings:

image(r, main = "Ratings")

# Create a recommender (model)

# There are a couple of different algorithms in the package:

# UBCF: User-based collaborative filtering
# IBCF: Item-based collaborative filtering

# Let's build one of each.

rec.ubcf <- Recommender(r, "UBCF", param=list(normalize="Z-score", method="Cosine"))
rec.ibcf <- Recommender(r, "IBCF", param=list(normalize="Z-score", method="Cosine"))

# Create Predictions

# The `predict` method will predict ratings for all the movies that users did not rate.
# We can then turn it into a data frame, and look at the predictions for each user.

# Also, let's create a dataframe with predictions from the UBCF and IBCF models, so we can plot.

pred.ubcf <- predict(rec.ubcf, r, type="ratings")
df.pred.ubcf <- as(pred.ubcf, "data.frame")

pred.ibcf <- predict(rec.ibcf, r, type="ratings")
df.pred.ibcf <- as(pred.ibcf, "data.frame")

df.pred.both <- df.pred.ubcf %>%
full_join(df.pred.ibcf, by=c("user", "item"), suffix=c("_ubcf", "_ibcf"))

# Look at some of the actual predictions

head(df.pred.both, n=50)

# Visualize the two models' predictions

ggplot(df.pred.both, aes(rating_ubcf, rating_ibcf)) + geom_point()

# Evaluation

# Now, let's get serious. Let's build a bunch of different models, and see which parameters lead to the best performance.

# The `recommenderlab` has it's own peculiar way of splitting data; we'll roll with it.

# First, to avoid repeating code, let's build a function that will take some parameters, build a recommendation model, and measure the performance.

buildAndEvaluateModel = function(ratings, model_name, normalize, method) {

e <- evaluationScheme(ratings, method="split", train=0.8, given=10)

start <- Sys.time()
model <- Recommender(getData(e, "train"), "UBCF", param=list(normalize, method))
train_time <- Sys.time() - start

start <- Sys.time()
prediction <- predict(model, getData(e, "known"), type="ratings")
predict_time <- Sys.time() - start

rmse <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]

return(c(rmse, train_time, predict_time))
}

# Now, let's define a bunch of combinations.

model_names = c("UBCF", "IBCF")
normalizes = c("Z-score", "center")
methods = c("Cosine", "Jaccard")
params = expand.grid(model_names, normalizes, methods)
colnames(params) = c("Model Name", "Normalize", "Method")
params$`Model Name`= as.character(params$`Model Name`)

res = data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = False)
colnames(res) = c(colnames(params), "rmse", "train_time", "predict_time")

for (i in 1:nrow(params)) {
  results = buildAndEvaluateModel(r, params[i, 1], params[i, 2], params[i, 3])
  res[nrow(res)+1,] = c(params[i,], results)
}

res %>% arrange(rmse)
