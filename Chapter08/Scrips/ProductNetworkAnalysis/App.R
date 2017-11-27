
library(shiny)
library(arules)
library(igraph, quietly = TRUE)


server <- function(input, output) {
  
  
  trans.obj <- reactive({
    data <- input$datafile
    transactions.obj <- read.transactions(file = data$datapath, format = "single", 
                                          sep = ",",
                                          cols = c("order_id", "product_id"), 
                                          rm.duplicates = FALSE,
                                          quote = "", skip = 0,
                                          encoding = "unknown")
    transactions.obj
    
    
  })
  
  trans.df <- reactive({
    
    data <- input$datafile
    if(is.null(data)){return(NULL)}
    trans.df <- read.csv(data$datapath)
    return(trans.df)
  })
  
  network.data <- reactive({
    transactions.obj <- trans.obj()
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
    
    # Clean up for item pairs
    library(tidyr)
    freq.items.df <- separate(data = freq.items.df, col = item_set, into = c("item.1", "item.2"), sep = ",")
    freq.items.df[] <- lapply(freq.items.df, gsub, pattern='\\{', replacement='')
    freq.items.df[] <- lapply(freq.items.df, gsub, pattern='\\}', replacement='')
    
    # Prepare data for graph
    network.data <- freq.items.df[,c('item.1','item.2','support.count')]
    names(network.data) <- c("from","to","weight")
    return(network.data)
    
  })
  
  output$transactions <- renderDataTable({
    trans.df()
  })
  output$ppairs <- renderDataTable({
    
    network.data()
    
  })
  
  output$community <- renderPlot({
    network.data <- network.data()
    my.graph <- graph_from_data_frame(network.data)
    random.cluster <- walktrap.community(my.graph)
    plot(random.cluster,my.graph,
         layout=layout.fruchterman.reingold,
         vertex.label.cex=.5,
         edge.arrow.size=.1,height = 1200, width = 1200)
  })

  
}

ui <- fluidPage(
  navbarPage("Product Pairs",
             tabPanel("Transactions"
                      , fileInput("datafile", "select transactions csv file",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                                    )
                      )
                      , dataTableOutput("transactions")
             ),
             tabPanel("Product Pairs"
                      ,dataTableOutput("ppairs")),
             tabPanel("Community"
                      ,plotOutput("community"))
  )
)

shinyApp(ui = ui, server = server)





