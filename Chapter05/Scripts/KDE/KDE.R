
# Generate data
data <- rnorm(1000, mean=25, sd=5)
data.1 <- rnorm(1000, mean=10, sd=2)
data <- c(data, data.1)

# Plot histogram
hist(data)
# View the bins
hist(data, plot = FALSE)

# Histogram with modified bins
hist(data, breaks = seq(0,50,2))



# Kernel Density estimation
kde = density(data)
plot(kde)


library(pdfCluster)
kde.1  <- kepdf(data)
plot(kde.1)
kde.1@kernel
kde.1@par



# Twitter Text
library(twitteR, quietly = TRUE)

consumer.key <- "NpQ32UlsqzDjNgHeOSeRW2Zaz"
consumer.secret <- "pZLjJPu8g75SqSMSTV4pKevHFUkhBx28dqd2EGlzR7g6qcTJHy"
access.token <- "4342772599-V8IS8s5L2lYImXNlGJn2glYPwiwBdBE0usm4jnA"
token.secret <- "iIo3a2uj4ogqDgmoq1SeQeFQH2nqgXjfKQ1mKCfiVJvTd"

setup_twitter_oauth(consumer.key, consumer.secret, access.token, token.secret)




blade_runner <- searchTwitter("#bladeRunner", n=100,lang = "en")
tweet.df <- twListToDF(blade_runner)
head(tweet.df)

tweet.df <- tweet.df[tweet.df$isRetweet == FALSE, ]
head(tweet.df)

tweet.df <- data.frame(tweet.df['text'])
head(tweet.df)

# Remove non graphical characters
library(stringr)
tweet.df$text =str_replace_all(tweet.df$text,"[\\.\\,\\;]+", " ")
tweet.df$text =str_replace_all(tweet.df$text,"http\\w+", "")
tweet.df$text =str_replace_all(tweet.df$text,"@\\w+", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[[:punct:]]", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[[:digit:]]", " ")
tweet.df$text =str_replace_all(tweet.df$text,"^ ", " ")
tweet.df$text =str_replace_all(tweet.df$text,"[<].*[>]", " ") 

head(tweet.df)

# Data gold set prepration
library(sentimentr, quietly = TRUE)
sentiment.score <- sentiment(tweet.df$text)
head(sentiment.score)

library(dplyr, quietly = TRUE)
sentiment.score <- sentiment.score %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))
head(sentiment.score)


tweet.df$polarity <- sentiment.score$sentiment
tweet.final <- tweet.df[,c('text','polarity')]
head(tweet.final)

tweet.final$sentiment <- ifelse(tweet.final$polarity <0, "Negative","Positive")
head(tweet.final)
table(tweet.final$sentiment)


head(tweet.final)

library(caret, quietly = TRUE)
tweet.final$sentiment <- as.factor(tweet.final$sentiment)
tweet.final <- upSample(x = tweet.final$text, y = tweet.final$sentiment)
names(tweet.final) <- c('text', 'sentiment')
head(tweet.final)
table(tweet.final$sentiment)
tweet.final$id <- seq(1, nrow(tweet.final))



# KDE Classifier
library(tm)
get.dtm <- function(text.col, id.col, input.df, weighting){

  title.reader <- readTabular(mapping=list(content=text.col, id=id.col))
  corpus <- Corpus(DataframeSource(input.df), readerControl=list(reader=title.reader))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weighting))
  return(dtm)
  
  
}

stopwords("english")

dtm <- get.dtm('text','id', tweet.final, "weightTfIdf")
dtm
dtm.mat <- as.matrix(dtm)
dtm.mat[1:2,10:15]

dtm <- get.dtm('text','id', tweet.final, "weightTf")
dtm.mat <- as.matrix(dtm)

# delta tf-idf 
dtm.pos <- get.dtm('text','id', tweet.final[tweet.final$sentiment == 'Positive',],"weightBin")
dtm.neg <- get.dtm('text','id', tweet.final[tweet.final$sentiment == 'Negative',],"weightBin")

dtm.pos.mat <- as.matrix(dtm.pos)
dtm.neg.mat <- as.matrix(dtm.neg)

pos.words.df <- colSums(dtm.pos.mat)
neg.words.df <- colSums(dtm.neg.mat)

pos.features <- colnames(dtm.pos.mat)
neg.features <- colnames(dtm.neg.mat)
tot.features <- unique(c(pos.features, neg.features))
doc.ids <- rownames(dtm.mat)

for( i in 1:length(tot.features)){
  for ( j in 1:length(doc.ids)){
    # Number of times the term has occured in the document
    ctd <- dtm.mat[doc.ids[j], tot.features[i]]
    # Number for documents in pos data with the term
    pt <- pos.words.df[tot.features[i]]
    # Number for documents in pos data with the term
    nt <- neg.words.df[tot.features[i]]
    score <- ctd * log( nt / pt)
    if(is.na(score)){
      score <- 0
    }
    dtm.mat[doc.ids[j], tot.features[i]] <- score
  }
  
}


  
library(naivebayes)
model <- naive_bayes(x = dtm.mat, y = tweet.final$sentiment, usekernel = TRUE, fl = 0.001)
model$prior
predict(model, newdata = dtm.mat[1:2, ], type = "prob")


