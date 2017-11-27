
library(shiny)
library(stream)
library(plotly)


sensor.1 <- DSD_Gaussians(k = 3, d = 2, noise = .05)

refreshSensor <- function(){
  return(get_points(sensor.1, n = 10, class = FALSE))
}

check.fun <- function(){
  Sys.sleep(0.05)
  return(rnorm(n=1))
  }

server <- function(input, output, session) {
  
  sample.data <- reactivePoll(50, session,check.fun,refreshSensor)    
  
  output$sensor.1 <- renderDataTable({
    data <- sample.data()
    data
  })
  output$sensor.1.plot <- renderPlot({
    data <- sample.data()
    plot_ly(data = data, x = ~X1, y = ~X2)
  })

  
}

ui <- fluidPage(
  navbarPage("Streaming data",
             tabPanel("Sensor.1"
                      , plotOutput("sensor.1.plot")
                      , dataTableOutput("sensor.1")
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





