#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data(mtcars)
  
  # display convenience
  mtcars$trans <- ifelse(mtcars$am == 0,"auto", "manual")
  
  # build the model
  model <- lm(mpg ~ wt + qsec + am, data=mtcars)
  
  # input vars
  am <- reactive({as.numeric(input$am)})
  wt <- reactive({as.numeric(input$wt)})
  qsec <- reactive({as.numeric(input$qsec)})
  
  # prediction
  pred <- reactive({
    predict(model, data.frame(wt=wt(), qsec=qsec(), am=am(), trans=ifelse(am() == 0,"auto", "manual")))
  })

  # model details  
  output$model_details <- renderPrint({
    summary(model)
  })
  
  # the predicted MPG from the input
  output$pred <- renderText({paste("Your Predicted MPG: ", pred())})
  
  # plot of all the data, plus the newly predicted point
  output$plot_pred <- renderPlotly({
    my <- data.frame(wt=wt(), qsec=qsec(), am=am(), trans=ifelse(am() == 0,"auto", "manual"))
    my$mpg <- predict(model, my)
    
    plot_ly(x = mtcars$wt, y = mtcars$mpg, 
      text=~paste('Model: ', rownames(mtcars), ', qsec: ', mtcars$qsec, ',', mtcars$trans), 
      hoverinfo = 'text', mode="markers",  type="scatter", color=as.factor(mtcars$trans), size=mtcars$qsec) %>%
    # adding the predicted value
    add_markers(x = my$wt, y = my$mpg,
      marker=list(symbol="x", size=10 , color="red" , opacity=0.7, maxdisplayed=1),
      text=~paste('MPG: ', my$mpg, 'Weight:', my$wt, ', qsec: ', my$qsec, ',', my$trans), 
      hoverinfo = 'text', mode="markers",  type="scatter", color=as.factor(my$trans), size=my$qsec) %>%
    layout(
      title = "Your MPG as related to weight, transmission, and 1/4 mile time of all cars",
      xaxis = list(title = "Weight x1000lbs"),
      yaxis = list(title = "MPG")
    )  
  })

})
