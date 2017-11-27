###############################################################################
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
#   RScript to explain application of hits to 
#   transaction database.
#
#   Gopi Subramanian
###############################################################################



library(arules)


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


## Create txn object
columns <- c("order_id", "product_id") ## columns of interest in data file
data.path = '../../data/data.csv'  ## Path to data file
transactions.obj <- get.txn(data.path, columns) ## create txn object

## Generate weight vector using hits
weights.vector <- hits( transactions.obj, type = "relative")
weights.df <- data.frame(transactionID = labels(weights.vector), weight = weights.vector)

head(weights.df)

transactions.obj@itemsetInfo <- weights.df

## Frequent item sets generation
support <- 0.01
parameters = list(
  support = support,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 10, # Maximal number of items per item set
  target  = "frequent itemsets"
  
)
weclat.itemsets <- weclat(transactions.obj, parameter = parameters)
weclat.itemsets.df <-data.frame(weclat.itemsets = labels(weclat.itemsets)
                                , weclat.itemsets@quality)

head(weclat.itemsets.df)
tail(weclat.itemsets.df)

## Rule induction
weclat.rules <- ruleInduction(weclat.itemsets, transactions.obj, confidence = 0.3)
weclat.rules.df <-data.frame(weclat.rules = labels(weclat.rules)
                             , weclat.rules@quality)

head(weclat.rules.df)


freq.weights <-  head(sort(itemFrequency(transactions.obj, weighted = TRUE),decreasing = TRUE),20)
freq.nweights <- head(sort(itemFrequency(transactions.obj, weighted = FALSE),decreasing = TRUE),20)

compare.df <- data.frame("items" =  names(freq.weights), 
                         "score" = freq.weights,
                         "items.nw" = names(freq.nweights),
                         "score.nw" = freq.nweights)
row.names(compare.df) <- NULL


