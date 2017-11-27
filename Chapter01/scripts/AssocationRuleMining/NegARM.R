########################################################################
#   
#   R Data Analysis Projects
#
#   Chapter 1
#
#   Building Recommender System
#   A step step approach to build Assocation Rule Mining 
#
#   Script:
#
#   RScript to explain negative associative rule mining
#
#   Gopi Subramanian
#########################################################################

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
get.neg.rules <- function(transactions, itemList, support, confidence){
  # Generate negative association rules for given support confidence value
  #
  # Args:
  #  transactions: Transaction object, list of transactions
  #  itemList : list of items to be negated in the transactions
  #  support: Minimum support threshold
  #  confidence: Minimum confidence threshold
  # Returns:
  #  A data frame with the best set negative rules and their support and confidence values
  neg.transactions <- addComplement( transactions, labels = itemList)
  rules <- get.rules(support, confidence, neg.transactions)
  return(rules)
}


columns <- c("order_id", "product_id") ## columns of interest in data file
data.path = '../../data/data.csv'  ## Path to data file

transactions.obj <- get.txn(data.path, columns) ## create txn object

itemList <- c("Organic Whole Milk","Cucumber Kirby")

neg.rules <- get.neg.rules(transactions.obj,itemList, support = .05, 
                           confidence = .6)

neg.rules.nr <- neg.rules[!is.redundant(neg.rules)]

labels(neg.rules.nr)
