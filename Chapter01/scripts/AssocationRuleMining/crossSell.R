###############################################################################
#   
#   R Data Analysis Projects
#
#   Chapter 1
#
#   Building Recommender System
#   A step step approach to build Assocation Rule Mining 
#
#
#   Script:
#      Generating rules for cross sell campaign.
#
#
#   Gopi Subramanian
###############################################################################

library(arules)
library(igraph)


get.txn <- function(data.path, columns){
  # Get transaction object for a given data file
  #
  # Args:
  #  data.path:  data file name location
  #  columns: transaction id and item id columns.
  #
  # Returns:
  #   transaction object
  transactions.obj <- read.transactions(file = data.path, format = "single", 
                                        sep = ",",
                                        cols = columns, 
                                        rm.duplicates = FALSE,
                                        quote = "", skip = 0,
                                        encoding = "unknown")
  return(transactions.obj)
}


get.rules <- function(support, confidence, transactions){
  # Get Apriori rules for given support and confidence values
  #
  # Args:
  #  support: support parameter
  #  confidence: confidence parameter
  #
  # Returns:
  #  rules object
  parameters = list(
    support = support,
    confidence = confidence,
    minlen  = 2,  # Minimal number of items per item set
    maxlen  = 10, # Maximal number of items per item set
    target  = "rules"
    
  )
  
  rules <- apriori(transactions, parameter = parameters)
  return(rules)
}

find.rules <- function(transactions, support, confidence, topN = 10){
  # Generate and prune the rules for given support confidence value
  #
  # Args:
  #  transactions: Transaction object, list of transactions
  #  support: Minimum support threshold
  #  confidence: Minimum confidence threshold
  # Returns:
  #  A data frame with the best set of rules and their support and confidence values
  
  
  # Get rules for given combination of support and confidence
  all.rules <- get.rules(support, confidence, transactions)
  
  rules.df <-data.frame(rules = labels(all.rules)
                        , all.rules@quality)
  
  other.im <- interestMeasure(all.rules, transactions = transactions)
  
  rules.df <- cbind(rules.df, other.im[,c('conviction','leverage')])
  
  
  # Keep the best rule based on the interest measure
  best.rules.df <- head(rules.df[order(-rules.df$leverage),],topN)
  
  return(best.rules.df)
}

plot.graph <- function(cross.sell.rules){
  # Plot the associated items as graph
  #
  # Args:
  #  cross.sell.rules: Set of final rules recommended
  # Returns:
  #  None
  edges <- unlist(lapply(cross.sell.rules['rules'], strsplit, split='=>'))
  
  g <- graph(edges = edges)
  plot(g)
  
}

support <- 0.01
confidence <- 0.2

columns <- c("order_id", "product_id") ## columns of interest in data file
data.path = '../../data/data.csv'  ## Path to data file

transactions.obj <- get.txn(data.path, columns) ## create txn object

cross.sell.rules <- find.rules( transactions.obj, support, confidence )
cross.sell.rules$rules <- as.character(cross.sell.rules$rules)              

plot.graph(cross.sell.rules)
