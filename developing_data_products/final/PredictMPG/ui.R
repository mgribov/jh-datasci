#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

shinyUI(
  navbarPage("Predict MPG for a car based on some key parameters",
           
    # input and prediction based on model
    tabPanel("Prediction",
      fluidPage(
        titlePanel("Enter your car's characteristics"),
        
        sidebarLayout(
        
          # input
          sidebarPanel(
            selectInput("am", "Transmission", c("Automatic" = "0", "Manual" = "1")),
            textInput("wt", "Weight (x1000lbs)", "3"), 
            textInput("qsec", "1/4 mile time (seconds)", "17")
          ),
            
          # prediction output
          mainPanel(
            h3(textOutput("Main Panel")),
              
            tabsetPanel(
              type = "tabs", 
              tabPanel("Predicted MPG", verbatimTextOutput("pred"), plotlyOutput("plot_pred")),
              tabPanel("Raw Model Details", verbatimTextOutput("model_details"))
            )
          )
        )
      )
    ),
    
    # description of the method/model    
    tabPanel("Usage, model, and methodology",
      h2("Predicting car's MPG value"),
      hr(),
      h3("Usage"),
      p("Use the input fields to enter your car's weight, transmission type, and 1/4 mile distance time going from 0mph."),
      p("The resulting predicted MPG value is based on a linear model trained on mtcars dataset."),
      p("You can see the raw model summary on 'Raw Model Details' tab"),
      h3("Model"),
      p("The most optimal linear model, using weight ('wt'), 1/4 mile time ('qsec'), and transmission type ('am') columns, was found as part of the final project for Regression Models class."),
      p("It was built using built-on lm() function."),
      h3("Data"),
      p("The linear model is built on mtcars dataset: https://vincentarelbundock.github.io/Rdatasets/doc/datasets/mtcars.html")
    )
  )
)
