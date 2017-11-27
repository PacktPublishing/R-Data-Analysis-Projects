# If you need to simulate some weights.
l <- nrow(transactions.obj) 
weights <- rnorm(l, mean = 0.6, sd = 0.3)
hist(weights)
weights.df <- transactions.obj@itemsetInfo
weights.df$weight <- weights
write.csv(weights.df,'../../data/weights.csv',row.names = FALSE )

##########################################
