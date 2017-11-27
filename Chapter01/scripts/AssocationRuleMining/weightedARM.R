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
#   RScript to explain weighted assocation rule mining
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


columns <- c("order_id", "product_id") ## columns of interest in data file
data.path = '../../data/data.csv'  ## Path to data file
transactions.obj <- get.txn(data.path, columns) ## create txn object

# Update the transaction objects
# with transaction weights
transactions.obj@itemsetInfo$weight <- NULL
# Read the weights file
weights <- read.csv('../../data/weights.csv')
transactions.obj@itemsetInfo <- weights

# Frequent item set generation
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

# Rule induction
weclat.rules <- ruleInduction(weclat.itemsets, transactions.obj, confidence = 0.3)
weclat.rules.df <-data.frame(rules = labels(weclat.rules)
                             , weclat.rules@quality)
head(weclat.rules.df)

weclat.rules.df$rules <- as.character(weclat.rules.df$rules)
plot.graph(weclat.rules.df)
