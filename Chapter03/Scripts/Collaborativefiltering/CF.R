set.seed(100)

products <- c('A','B','C','D','E','F','G')
user.a <-   c( 3,  0,  2, 5, 5, 0,1)
user.b <-   c( 3,  5,  3, 5, 4, 2, 1)

ratings.matrix <- as.data.frame(list(user.a,user.b))
names(ratings.matrix) <- c("user.a","user.b")
rownames(ratings.matrix) <- products
head(ratings.matrix)

products <- c('A','B','C')
user.a <- c(2,0,3)
user.b <- c(5,2,0)
user.c <- c(3,3,0)
ratings.matrix <- as.data.frame(list(user.a,user.b, user.c))
names(ratings.matrix) <- c("user.a","user.b","user.c")
rownames(ratings.matrix) <- products
head(ratings.matrix)


ratings.mat <- (as.matrix(ratings.matrix))
sim.mat <- cor(t(ratings.mat), method = "pearson")
sim.mat

svd.coms <- svd(ratings.mat)
user.a.vector <- svd.coms$u[1,]
product.B.vector <- svd.coms$v[2,]

ratings.user.a.prod.B <- user.a.vector %*% product.B.vector
ratings.user.a.prod.B


svd.coms$u[1,] %*% svd.coms$v[2,]

install.packages("recommenderlab")


library(recommenderlab, quietly = TRUE)

# Binary Rating Matrix
bin.data <- sample(c(0,1), 20, replace = TRUE)
bin.mat <- matrix(bin.data, nrow = 4, ncol = 5)
bin.mat
rating.mat <- as(bin.mat, "binaryRatingMatrix")
rating.mat

# A quick recommender
model <- Recommender(data = rating.mat, method = "POPULAR")
model
str(model)

recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")

# Quick Prediction
recomms <- predict(model, newdata = rating.mat, n =2)
recomms@items
recomms@ratings


data("Jester5k")
str(Jester5k)
head(Jester5k@data[1:5,1:5])


# Users
Jester5k@data[1,]
Jester5k@data[100,]

zero.ratings <- rowSums(Jester5k@data == 0)
zero.ratings.df <- data.frame("user" = names(zero.ratings), "count" = zero.ratings)
head(zero.ratings.df)
head(zero.ratings.df[order(-zero.ratings.df$count),], 10)
hist(zero.ratings.df$count, main ="Distribution of zero rated jokes")
zero.density <- density(zero.ratings.df$count)
plot(zero.density)

model <- kmeans(zero.ratings.df$count,3 )
model$centers
model$size
model.df <- data.frame(centers = model$centers, size = model$size, perc = (model$size / 5000) * 100)
head(model.df)


# Ratings
Jester5k@data[,1]

par(mfrow=c(2,2))
joke.density <- density(Jester5k@data[,1])
plot(joke.density)

joke.density <- density(Jester5k@data[,25])
plot(joke.density)

joke.density <- density(Jester5k@data[,70])
plot(joke.density)

joke.density <- density(Jester5k@data[,100])
plot(joke.density)


par(mfrow=c(1,1))
nratings(Jester5k)
hist(getRatings(Jester5k), main="Distribution of ratings")

# Popular joke
# Binarize the ratings
ratings.binary <- binarize(Jester5k, minRating =0)
ratings.binary
ratings.sum <- colSums(ratings.binary)
ratings.sum.df <- data.frame(joke = names(ratings.sum), pratings = ratings.sum)
head( ratings.sum.df[order(-ratings.sum.df$pratings), ],10)
tail( ratings.sum.df[order(-ratings.sum.df$pratings), ],10)



# Sample
data <- sample(Jester5k, 1500)
hist(getRatings(data), main="Distribution of ratings for 1500 users")


##
data <- sample(Jester5k, 1500)
ratings.mat <- getRatingMatrix(data)
str(ratings.mat)

str(data@data)


# Normalize the data
data.norm   <- normalize(data, method = "center")
data.norm.z <- normalize(data, method = "Z-score")

par(mfrow=c(3,1))
plot(density(getRatings(data)), main = "Raw")
plot(density(getRatings(data.norm)), main = "Normalized")
plot(density(getRatings(data.norm.z)), main = "Z-score normalized")
par(mfrow=c(1,1))


# Train test split
plan <- evaluationScheme(data, method="split", train=0.9, given = 10, goodRating=5)
plan
train <- getData(plan, "train")
train
test <- getData(plan, "unknown")
test
test.known <- getData(plan, "known")
test.known

# Look at the data
dim(train@data)
dim(test@data)


# Base line model
random.model <- Recommender(train, "RANDOM")
random.model
getModel(random.model)

random.predict <- predict(random.model, test.known, n = 5, type = "topNList")
random.predict@items[1]
random.predict@ratings[1]

test@data[1,]


# Popular Model
popular.model <- Recommender(train, "POPULAR")
popular.model

popular.predict <- predict(popular.model, test.known, n = 5, type = "topNList")
popular.predict@items[1]
popular.predict@ratings[1]

test@data[1,c(50,32,36,27,53)]



# Evaluate the results
results <- evaluate(plan, method = "POPULAR", type = "topNList", n = 5 )
getConfusionMatrix(results)


# Cross validation
plan <- evaluationScheme(data, method="cross", train=0.9, given = 10, goodRating=5)
results <- evaluate(plan, method = "POPULAR", type = "topNList", n = c(5,10,15) )
avg(results)


# User based models
plan <- evaluationScheme(data, method="cross", train=0.9, given = 10, goodRating=5)
results <- evaluate(plan, method = "UBCF", type = "topNList", n = c(5,10,15) )
avg(results)

# Item based models
plan <- evaluationScheme(data, method="cross", train=0.9, given = 10, goodRating=5)
results <- evaluate(plan, method = "IBCF", type = "topNList", n = c(5,10,15) )
avg(results)

# factor based models
plan <- evaluationScheme(data, method="cross", train=0.9, given = 10, goodRating=5)
results <- evaluate(plan, method = "SVDF", type = "topNList", n = c(5,10,15) )
avg(results)




