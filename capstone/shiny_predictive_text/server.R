library(shiny)
library(dplyr)
library(data.table)

source("functions.R")

shinyServer(function(input, output) {
  n_pred <- reactive({as.numeric(input$words)})
  phrase <- reactive({input$phrase})
  
  pred <- reactive({predict_word(phrase(), n_pred())})
  
  output$pred <- renderText({paste("Your Predicted Words: ", paste(pred(), collapse=", "))})

})
