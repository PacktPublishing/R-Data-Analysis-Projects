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
#     A simple experiment to find support and confidence values
#
#
#   Gopi Subramanian
###############################################################################

library(ggplot2)
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

explore.parameters <- function(transactions){
  # Explore support and confidence space for the given transactions
  #
  # Args:
  #  transactions: Transaction object, list of transactions
  # 
  # Returns:
  #  A data frame with no of rules generated for a given
  #  support confidence pair.
  
  support.values <- seq(from = 0.001, to = 0.1, by = 0.001)
  confidence.values <- seq(from = 0.05, to = 0.1, by = 0.01)
  support.confidence <- expand.grid(support = support.values, 
                                    confidence = confidence.values)
  
  # Get rules for various combinations of support and confidence
  rules.grid <- apply(support.confidence[,c('support','confidence')], 1, 
                      function(x) get.rules(x['support'], x['confidence'], transactions))
  
  no.rules <- sapply(seq_along(rules.grid),
                     function(i) length(labels(rules.grid[[i]])))

  no.txn <- sapply(seq_along(rules.grid),
                   function(i) length(as(supportingTransactions(rules.grid[[i]], 
                                                                transactions), "list")))
  
  no.rules.df <- data.frame(support.confidence, no.rules, no.txn)
  return(no.rules.df)
  
  
}

get.plots <- function(no.rules.df){
  # Plot the number of rules generated for 
  # different support and confidence thresholds
  #
  # Args:
  #  no.rules.df :  data frame of number of rules
  #                 for different support and confidence
  #                 values
  #
  # Returns:
  #   None
  
  exp.plot  <- function(confidence.value){
    print(ggplot(no.rules.df[no.rules.df$confidence == confidence.value,],
           aes(support, no.rules), environment = environment()) + geom_line()
          + ggtitle(paste("confidence = ", confidence.value)))
  }
  confidence.values <- c(0.07,0.08,0.09,0.1)
  mapply(exp.plot, confidence.value = confidence.values)

}

columns <- c("order_id", "product_id") ## columns of interest in data file
data.path = '../../data/data.csv'  ## Path to data file

transactions.obj <- get.txn(data.path, columns) ## create txn object
no.rules.df <- explore.parameters(transactions.obj) ## explore number of rules

head(no.rules.df) ## 

get.plots(no.rules.df) ## Plot no of rules vs support

