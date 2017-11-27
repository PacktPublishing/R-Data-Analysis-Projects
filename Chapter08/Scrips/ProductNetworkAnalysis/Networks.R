########################################################################
#   
#   R Data Analysis Projects
#
#   Chapter X
#
#   Product Network Analysis
#   Leverage Graph Networks to identify product categories
#
#   
#
#   Gopi Subramanian
#########################################################################

library(igraph, quietly = TRUE)

## Creating a simple graph
simple.graph <- graph_from_literal(A-B, B-C, C-D, E-F, A-E, E-C)
plot.igraph(simple.graph)

# Directed graph
directed.graph <- graph_from_literal(A-+B, B-+C, C+-D, E-+F, A+-E, E-+C)
plot.igraph(directed.graph)

## Graph properties
E(simple.graph) # Edges
V(simple.graph) # Vertices

E(directed.graph) # Edges
V(directed.graph) # Vertices


## Graph attributes
V(simple.graph)$name <- c('alice', 'bob','charlie','david', 'eli','francis')
simple.graph <- set_vertex_attr(simple.graph ,"age", value = c('11', '11','15','9', '8','11'))
plot.igraph(simple.graph)
V(simple.graph)$color <- ifelse(V(simple.graph)$age == '11', "blue","green")
plot.igraph(simple.graph)

# Structural properties
degree(simple.graph) # degree of nodes
E(simple.graph)$weight <- c(10,20,35,15,25,35)
strength(simple.graph) # strength of nodes
get.adjacency(simple.graph) # adjacency matrix


url <- "http://sites.google.com/site/cxnets/US_largest500_airportnetwork.txt"
tmp <- tempdir()
dest <- paste(sep="", tmp, "/", "usairport.txt")
download.file(url, dest)
usairport <- read_graph(dest, format="ncol")
usairport$name <- "US air transportation network"
usairport$Author <- "V. Colizza, R. Pastor-Satorras and A. Vespignani"
usairport$Citation <- "V. Colizza, R. Pastor-Satorras and A. Vespignani. Reaction-diffusion processes and metapopulation models in heterogeneous networks. Nature Physics 3, 276-282 (2007)."
usairport$URL <- "http://sites.google.com/site/cxnets/usairtransportationnetwork"

degrees <- degree(usairport, loops = FALSE)
head(sort(degrees, decreasing = TRUE),10)

# Closeness
head(sort(closeness(usairport), decreasing = TRUE),10)

# Delete edges and nodes
simple.graph <- delete.edges(simple.graph, "alice|bob" )
simple.graph <- delete.vertices(simple.graph, 'francis')
plot(simple.graph)

# graph algorithms
shortest.paths(simple.graph, "alice")
random_walk(simple.graph, start = "alice", steps = 3)

# use case date
data <- read.csv('data.csv')
head(data)

## Prepare the data
library(arules)
transactions.obj <- read.transactions(file = 'data.csv', format = "single", 
                                      sep = ",",
                                      cols = c("order_id", "product_id"), 
                                      rm.duplicates = FALSE,
                                      quote = "", skip = 0,
                                      encoding = "unknown")
transactions.obj

# Interest Measures
support    <- 0.015

# Frequent item sets
parameters = list(
  support = support,
  minlen  = 2,  # Minimal number of items per item set
  maxlen  = 2, # Maximal number of items per item set
  target  = "frequent itemsets"
)

freq.items <- apriori(transactions.obj, parameter = parameters)

# Let us examine our freq item sites
freq.items.df <- data.frame(item_set = labels(freq.items)
                            , support = freq.items@quality)
freq.items.df$item_set <- as.character(freq.items.df$item_set)
head(freq.items.df)

# Clean up for item pairs
library(tidyr)
freq.items.df <- separate(data = freq.items.df, col = item_set, into = c("item.1", "item.2"), sep = ",")
freq.items.df[] <- lapply(freq.items.df, gsub, pattern='\\{', replacement='')
freq.items.df[] <- lapply(freq.items.df, gsub, pattern='\\}', replacement='')
head(freq.items.df)

# Prepare data for graph
network.data <- freq.items.df[,c('item.1','item.2','support.count')]
names(network.data) <- c("from","to","weight")
head(network.data)

## Build the graph
library(igraph, quietly = TRUE)
my.graph <- graph_from_data_frame(network.data)
plot.igraph(my.graph,
            layout=layout.fruchterman.reingold,
            vertex.label.cex=.5,
            edge.arrow.size=.1)


## Clustering
random.cluster <- walktrap.community(my.graph)
str(random.cluster)
random.cluster
groupings.df <- data.frame(products = random.cluster$names, group = random.cluster$membership)
head(groupings.df)
groupings.df[groupings.df$group == 2,]
groupings.df[groupings.df$group == 1,]

plot(random.cluster,my.graph,
            layout=layout.fruchterman.reingold,
            vertex.label.cex=.5,
            edge.arrow.size=.1)

get.adjacency(my.graph)
strength(my.graph)
degree(my.graph)

## Random walks
for(var in seq(1,5)){
  print (random_walk(my.graph, start = "Banana", steps = 2))
}

random_walk(my.graph, start = "Banana", steps = 3)
random_walk(my.graph, start = "Banana", steps = 4)
random_walk(my.graph, start = "Banana", steps = 5)





