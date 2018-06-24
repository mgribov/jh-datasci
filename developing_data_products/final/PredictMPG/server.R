#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data(mtcars)
  
  model <- lm(mpg ~ wt + qsec + am, data=mtcars)
  
  am <- reactive({as.numeric(input$am)})
  wt <- reactive({as.numeric(input$wt)})
  qsec <- reactive({as.numeric(input$qsec)})
  
  pred <- reactive({
    predict(model, data.frame(wt=wt(), qsec=qsec(), am=am()))
  })
  
  output$pred <- renderPrint({pred()})

})
