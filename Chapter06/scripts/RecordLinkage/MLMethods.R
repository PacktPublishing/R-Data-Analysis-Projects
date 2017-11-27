###############################################################################
#   
#   R Data Analysis Projects
#
#   Chapter X
#
#   Record Linkage
#   Stochastic and Machine Learning Approaches 

#
#   Gopi Subramanian
###############################################################################

library(RecordLinkage, quietly = TRUE)
data("RLdata500")

# weight calculation
rec.pairs <- compare.dedup(RLdata500
                           ,blockfld = list(1, 5:7)
                           ,strcmp =   c(2,3,4)
                           ,strcmpfun = levenshteinSim)

# Unsupervised classification
kmeans.model <- classifyUnsup(rec.pairs, method = "kmeans")
summary(kmeans.model)

final.results <- kmeans.model$pairs
final.results$prediction <- kmeans.model$prediction
head(final.results)


# Supervised Learning 1
str(identity.RLdata500)
rec.pairs <- compare.dedup(RLdata500
                           ,identity = identity.RLdata500
                           ,blockfld = list(1, 5:7)
)
head(rec.pairs$pairs)

train <- getMinimalTrain(rec.pairs)
model <- trainSupv(train, method ="bagging")
train.pred <- classifySupv(model, newdata = train)
test.pred  <- classifySupv(model, newdata = rec.pairs)

summary(train.pred)
summary(test.pred)

# Supervised learning 2 
rec.pairs <- compare.dedup(RLdata500
                           ,blockfld = list(1, 5:7)
                           ,strcmp =   c(2,3,4)
                           ,strcmpfun = levenshteinSim)

# Run K-Means Model
kmeans.model <- classifyUnsup(rec.pairs, method = "kmeans")

# Change the original rec.pairs with rec.pairs from K-Means
pairs <- kmeans.model$pairs
pairs$prediction <- kmeans.model$prediction
head(pairs)

pairs$is_match <- NULL
pairs$is_match <- ifelse(pairs$prediction == 'N', 0,1)
pairs$prediction <- NULL
pairs[is.na(pairs)] <- 0
head(pairs)

rec.pairs$pairs <- pairs
head(rec.pairs$pairs)

train <- getMinimalTrain(rec.pairs)
model <- trainSupv(train, method ="bagging")
train.pred <- classifySupv(model, newdata = train)
test.pred  <- classifySupv(model, newdata = rec.pairs)

summary(train.pred)
summary(test.pred)





