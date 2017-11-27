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
#   RScript to explain how to use arules package
#   to mine assocation rules
#
#   Gopi Subramanian
###############################################################################

data.path = '../../data/data.csv'  
data = read.csv(data.path)      
head(data)

library(dplyr)
data %>% group_by('order_id') %>% summarize(order.count = n_distinct(order_id))
data %>% group_by('product_id') %>% summarize(product.count = n_distinct(product_id))


library(arules)

transactions.obj <- read.transactions(file = data.path, format = "single", 
                                      sep = ",",
                                      cols = c("order_id", "product_id"), 
                                      rm.duplicates = FALSE,
                                      quote = "", skip = 0,
                                      encoding = "unknown")
transactions.obj

# Other ways to create a transaction object
# From a dataframe


# Item frequency
as.data.frame(head(sort(itemFrequency(transactions.obj, type = "absolute")
          , decreasing = TRUE), 10) )  # Most  frequent

as.data.frame(head(sort(itemFrequency(transactions.obj, type = "absolute")
          , decreasing = FALSE), 10))  # Least frequent

itemFrequencyPlot(transactions.obj,topN = 25)

# Interest Measures
support    <- 0.01

# Frequent item sets
parameters = list(
  support = support,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 10, # Maximal number of items per item set
  target  = "frequent itemsets"
)

freq.items <- apriori(transactions.obj, parameter = parameters)
# Let us examine our freq item sites
freq.items.df <- data.frame(item_set = labels(freq.items)
                               , support = freq.items@quality)

head(freq.items.df, 5)
tail(freq.items.df, 5)


########### How to exclude certain items ####################
exclusion.items <- c('Banana','Bag of Organic Bananas')

freq.items <- apriori(transactions.obj, parameter = parameters,
                      appearance = list(none = exclusion.items,
                      default = "both"))
freq.items.df <- data.frame(item_set = labels(freq.items)
                            , support = freq.items@quality)


head(freq.items.df,10)

# Let us now examine the rules
confidence <- 0.2 # Interest Measure
parameters = list(
  support = support,
  confidence = confidence,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 10, # Maximal number of items per item set
  target  = "rules"
)

rules <- apriori(transactions.obj, parameter = parameters)
rules.df <- data.frame(rules = labels(rules)
                            ,rules@quality)

head(rules.df)
tail(rules.df)


interestMeasure(rules, transactions = transactions.obj)
rules.df <- cbind(rules.df, data.frame(interestMeasure(rules, 
                              transactions = transactions.obj)))

## Some sanity checks
duplicated(rules) # Any duplicate rules ?
is.significant(rules, transactions.obj)

rules[is.significant(rules, transactions.obj)]

## Transactions which support the rule.
as(supportingTransactions(rules, transactions.obj), "list")













