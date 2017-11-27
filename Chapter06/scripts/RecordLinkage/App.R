
library(shiny)
library(RecordLinkage)
data("RLdata500")



server <- function(input, output) {
  
  output$records <- renderDataTable({
    RLdata500
  })

  output$weights <- renderDataTable({
    rec.pairs <- compare.dedup(RLdata500
                               ,blockfld = list(1, 5:7)
                               ,strcmp =   c(2,3,4)
                               ,strcmpfun = levenshteinSim)
    pairs.weights <- emWeights(rec.pairs)
    
    pairs.classify <- emClassify(pairs.weights, threshold.upper = input$upperthreshold, threshold.lower = input$lowerthreshold)
    final.results <- pairs.classify$pairs
    final.results$weight <- pairs.classify$Wdata
    final.results$links <- pairs.classify$prediction
    
    final.results
    
  })
  output$weightplot <- renderPlot({
    rec.pairs <- compare.dedup(RLdata500
                               ,blockfld = list(1, 5:7)
                               ,strcmp =   c(2,3,4)
                               ,strcmpfun = levenshteinSim)
    pairs.weights <- epiWeights(rec.pairs)
    
    hist(pairs.weights$Wdata)
    
  })
}

ui <- fluidPage(
  navbarPage("Record Linkage",
             tabPanel("Load"
                      , dataTableOutput("records")
             ),
             tabPanel("Weights Method"
                      ,plotOutput("weightplot")
                      ,sliderInput("lowerthreshold", "Weight Lower threshold:",
                                         min = 0.0, max = 1.0,
                                         value =0.2)
                      ,sliderInput("upperthreshold", "Weight Upper threshold:",
                                   min = 0.0, max = 1.0,
                                   value =0.5)
                    
                      ,dataTableOutput("weights")
                      )
  )
)

shinyApp(ui = ui, server = server)





