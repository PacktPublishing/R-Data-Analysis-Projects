
library(shiny)
library(twitteR, quietly = TRUE)
library(stringr)
library(sentimentr, quietly = TRUE)
library(dplyr, quietly = TRUE)





consumer.key <- "NpQ32UlsqzDjNgHeOSeRW2Zaz"
consumer.secret <- "pZLjJPu8g75SqSMSTV4pKevHFUkhBx28dqd2EGlzR7g6qcTJHy"
access.token <- "4342772599-V8IS8s5L2lYImXNlGJn2glYPwiwBdBE0usm4jnA"
token.secret <- "iIo3a2uj4ogqDgmoq1SeQeFQH2nqgXjfKQ1mKCfiVJvTd"

setup_twitter_oauth(consumer.key, consumer.secret, access.token, token.secret)



server <- function(input, output) {
  
  tweet.df <- reactive({
    tweet.results <- searchTwitter(input$search, n=100,lang = "en")
    tweet.df <- twListToDF(tweet.results)
    # Remove retweets
    tweet.df <- tweet.df[tweet.df$isRetweet == FALSE, ]
    # Cleanup tweets
    tweet.df <- data.frame(tweet.df['text'])
    tweet.df$text =str_replace_all(tweet.df$text,"[\\.\\,\\;]+", " ")
    tweet.df$text =str_replace_all(tweet.df$text,"http\\w+", "")
    tweet.df$text =str_replace_all(tweet.df$text,"@\\w+", " ")
    tweet.df$text =str_replace_all(tweet.df$text,"[[:punct:]]", " ")
    tweet.df$text =str_replace_all(tweet.df$text,"[[:digit:]]", " ")
    tweet.df$text =str_replace_all(tweet.df$text,"^ ", " ")
    tweet.df$text =str_replace_all(tweet.df$text," $", " ")
    tweet.df$text =str_replace_all(tweet.df$text,"[<].*[>]", " ")     
    # Get sentiment
    sentiment.score <- sentiment(tweet.df$text)
    sentiment.score <- sentiment.score %>% group_by(element_id) %>% summarise(sentiment = mean(sentiment))
    tweet.df$polarity <- sentiment.score$sentiment
    tweet.df$sentiment <- ifelse(tweet.df$polarity <0, "Negative","Positive")
    
    return(tweet.df)
    
  })

  output$results <- renderDataTable({
    tweet.df()['text']
  })
  
  output$sentiment <- renderDataTable({
    tweet.df()
    
  })
  
}

ui <- fluidPage(
  navbarPage("TweetSenti",
             tabPanel("Search Tweets"
                      , textInput("search","search",value="#bladerunner")
                      , dataTableOutput("results")
                      ),
             tabPanel("Tag Sentiment"
                      ,dataTableOutput("sentiment"))
  )
)

shinyApp(ui = ui, server = server)





