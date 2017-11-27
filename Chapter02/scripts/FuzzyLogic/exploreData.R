# A toy example
library(data.table)

wine.data <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data')
head(wine.data)

table(wine.data$V1)

wine.type <- wine.data[,1]
wine.features <- wine.data[,-1]

wine.features.scaled <- data.frame(scale(wine.features))
wine.mat <- data.matrix(wine.features.scaled)

rownames(wine.mat) <- seq(1:dim(wine.features.scaled)[1])
wine.mat[1:2,]

wine.mat <- t(wine.mat)
cor.matrix <- cor(wine.mat, use = "pairwise.complete.obs", method = "pearson")
dim(cor.matrix)
cor.matrix[1:5,1:5]

user.view <- wine.features.scaled[3,]
user.view

sim.items <- cor.matrix[3,]
sim.items
sim.items.sorted <- sort(sim.items, decreasing = TRUE)
sim.items.sorted[1:5]

rbind(wine.data[3,]
,wine.data[52,]
,wine.data[51,]
,wine.data[85,]
,wine.data[15,]
)








library(tidyverse)
library(tidytext)
library(tm)
library(slam)


cnames <- c('ID' , 'TITLE' , 'URL' , 
            'PUBLISHER' , 'CATEGORY' , 
            'STORY' , 'HOSTNAME' ,'TIMESTAMP')

data <- read_tsv('newsCorpus.csv', 
                   col_names = cnames,
                   col_types = cols(
                   ID = col_integer(),
                   TITLE = col_character(),
                   URL = col_character(),
                   PUBLISHER = col_character(),
                   CATEGORY = col_character(),
                   STORY = col_character(),
                   HOSTNAME = col_character(),
                   TIMESTAMP = col_double()
                 )
                 )


head(data)
data %>% group_by(PUBLISHER) %>% summarise()
data %>% group_by(CATEGORY) %>% summarise()


publisher.count <- data.frame(data %>% group_by(PUBLISHER) %>% summarise(ct =n()))
head(publisher.count)
dim(publisher.count)
dim(publisher.count[publisher.count$ct <= 10,])


publisher.top <- head(publisher.count[order(-publisher.count$ct),],100)
head(publisher.top)

data.subset <- inner_join(publisher.top, data)
head(data.subset)
dim(data.subset)

title.df <- data.subset[,c('ID','TITLE')]
others.df <- data.subset[,c('ID','PUBLISHER','CATEGORY')]



######### Cosine Similarity #######################
title.reader <- readTabular(mapping=list(content="TITLE", id="ID"))

corpus <- Corpus(DataframeSource(title.df), readerControl=list(reader=title.reader))
corpus

getTransformations()

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

stopwords("english")

dtm <- DocumentTermMatrix(corpus, control=list(wordlenth = c(3,10) ,weighting = "weightTfIdf"))
dtm
inspect(dtm[1:5,10:15])

sim.score <- tcrossprod_simple_triplet_matrix(dtm)/(sqrt( row_sums(dtm^2) %*% t(row_sums(dtm^2)) ))
sim.score[1:10,1:10]

match.docs <- sim.score["38081",]
match.docs


match.df <- data.frame(ID = names(match.docs), cosine = match.docs, stringsAsFactors=FALSE)
match.df$ID <- as.integer(match.df$ID)
head(match.df)
match.refined<-head(match.df[order(-match.df$cosine),],30)
head(match.refined)



################## Polarity score ###############

match.refined <- inner_join(match.refined, title.df)
match.refined <- inner_join(match.refined, others.df)

head(match.refined)

library(dplyr, quietly = TRUE)
library(sentimentr, quietly = TRUE)
  
sentiment.score <- sentiment(match.refined$TITLE)
head(sentiment.score)

sentiment.score <- sentiment.score %>% group_by(element_id) %>%
  summarise(sentiment = mean(sentiment))
head(sentiment.score)

match.refined$polarity <- sentiment.score$sentiment
head(match.refined)
  
help("sentiment")
#
target.publisher <- match.refined[1,]$PUBLISHER
target.category <- match.refined[1,]$CATEGORY
target.polarity <- match.refined[1,]$polarity
target.title <- match.refined[1,]$TITLE


#match.refined <- match.refined[-1,]
match.refined$is.publisher <- match.refined$PUBLISHER == target.publisher
match.refined$is.publisher <- as.numeric(match.refined$is.publisher)

match.refined$is.category <- match.refined$CATEGORY == target.category
match.refined$is.category <- as.numeric(match.refined$is.category)

# Calcuate Jaccards
match.refined$jaccard <- (match.refined$is.publisher + match.refined$is.category)/2
match.refined$polaritydiff <- abs(target.sentiment - match.refined$polarity)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
match.refined$polaritydiff <- range01(unlist(match.refined$polaritydiff))


head(match.refined)
## clean up
match.refined$is.publisher = NULL
match.refined$is.category = NULL
match.refined$polarity = NULL
match.refined$sentiment = NULL


head(match.refined)


### Fuzzy Logic ########

library(sets, quietly = TRUE)
sets_options("universe", seq(from = 0,
                             to = 1, by = 0.1))


variables <-
  set(cosine =
        fuzzy_partition(varnames =
                          c(vlow = 0.2, low = 0.4,
                             medium = 0.6, high = 0.8),
                        FUN = fuzzy_cone , radius = 0.2),
        jaccard =
        fuzzy_partition(varnames =
                          c(close = 1.0, halfway = 0.5,
                            far = 0.0),
                        FUN = fuzzy_cone , radius = 0.4),
      
      polarity =
        fuzzy_partition(varnames =
                          c(same = 0.0, similar = 0.3,close = 0.5,
                            away = 0.7),
                        FUN = fuzzy_cone , radius = 0.2),
      ranking =
        fuzzy_partition(varnames =
                          c(H = 1.0, MED = 0.7 , M = 0.5, L = 0.3),
                        FUN = fuzzy_cone , radius = 0.2
                        )
  )

rules <-
  set(
    
    ######### Low Ranking Rules ###################
    fuzzy_rule(cosine %is% vlow, 
               ranking %is% L),
    
    fuzzy_rule(cosine %is% low || jaccard %is% far
               || polarity %is% away,
               ranking %is% L),
    
    fuzzy_rule(cosine %is% low || jaccard %is% halfway
               || polarity %is% away,
               ranking %is% L),

    fuzzy_rule(cosine %is% low || jaccard %is% halfway
               || polarity %is% close,
               ranking %is% L),
    
    fuzzy_rule(cosine %is% low || jaccard %is% halfway
               || polarity %is% similar,
               ranking %is% L),
        
    fuzzy_rule(cosine %is% low || jaccard %is% halfway
               || polarity %is% same,
               ranking %is% L),
    
    fuzzy_rule(cosine %is% medium || jaccard %is% far
               || polarity %is% away,
               ranking %is% L),
    
    ############### Medium Ranking Rules ##################
    
    
    fuzzy_rule(cosine %is% low || jaccard %is% close
               || polarity %is% same,
               ranking %is% M),
    
    fuzzy_rule(cosine %is% low && jaccard %is% close
               && polarity %is% similar,
               ranking %is% M),
    
    ############### Median Ranking Rule ##################
    
    
    fuzzy_rule(cosine %is% medium && jaccard %is% close
               && polarity %is% same,
               ranking %is% MED),
    
    fuzzy_rule(cosine %is% medium && jaccard %is% halfway
               && polarity %is% same,
               ranking %is% MED),
    
    fuzzy_rule(cosine %is% medium && jaccard %is% close
               && polarity %is% similar,
               ranking %is% MED),
    
        
    fuzzy_rule(cosine %is% medium && jaccard %is% halfway
               && polarity %is% similar,
               ranking %is% MED),
    
    
    ############## High Ranking Rule #####################
    
    fuzzy_rule(cosine %is% high,ranking %is% H)
    
    
    
  )

ranking.system <- fuzzy_system(variables, rules)
print(ranking.system)

plot(ranking.system)

fi <- fuzzy_inference(ranking.system, list(cosine = 0.5000000,  jaccard = 0, polarity=0.00000000))
gset_defuzzify(fi, "centroid")
plot(fi)

get.ranks <- function(dataframe){
  cosine =  as.numeric(dataframe['cosine'])
  jaccard = as.numeric(dataframe['jaccard'])
  polarity = as.numeric(dataframe['polaritydiff'])
  fi <- fuzzy_inference(ranking.system, list(cosine = cosine,  jaccard = jaccard, polarity=polarity))
  return(gset_defuzzify(fi, "centroid"))
  
}

match.refined$ranking <- apply(match.refined, 1, get.ranks)
match.refined <- match.refined[order(-match.refined$ranking),]
head(match.refined)



