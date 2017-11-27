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

library(RecordLinkage)
data("RLdata500")

# Em weight calculation
rec.pairs <- compare.dedup(RLdata500
                           ,blockfld = list(1, 5:7)
                           ,strcmp =   c(2,3,4)
                           ,strcmpfun = levenshteinSim)
pairs.weights <- emWeights(rec.pairs)
hist(pairs.weights$Wdata)

summary(pairs.weights)

weights.df<-getPairs(pairs.weights)
head(weights.df)

# Classification
pairs.classify <- emClassify(pairs.weights, threshold.upper = 10, threshold.lower = 5)

# View the matches
final.results <- pairs.classify$pairs
final.results$weight <- pairs.classify$Wdata
final.results$links <- pairs.classify$prediction
head(final.results)

counts <- table(final.results$links)
barplot(counts, main="Link Distribution",
        xlab="Link Types") 

# Final output to our customer
weights.df.srow <-getPairs( pairs.weights, single.rows = TRUE)
final.matches <- final.results[final.results$links == 'L',]

final <- merge(final.matches, weights.df.srow)
final <- subset(final, select = -c(fname_c1.2, fname_c2.2, lname_c1.2, lname_c2.2, by.2, bm.2, bd.2, weight))
head(final)



