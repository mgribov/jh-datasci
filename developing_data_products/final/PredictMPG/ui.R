#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

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
              tabPanel("Predicted MPG", verbatimTextOutput("pred")),
              tabPanel("Regression model", "Contains")
            )
          )
        )
      )
    ),
    
    # description of the method/model    
    tabPanel("The model and methodology",
      h2("Motor Trend Car Road Tests"),
      hr(),
      h3("Description"),
      helpText("The data was extracted from the 1974 Motor Trend US magazine,",
               " and comprises fuel consumption and 10 aspects of automobile design and performance",
               " for 32 automobiles (1973–74 models)."),
      h3("Format"),
      p("A data frame with 32 observations on 11 variables."),
      
      p("  [, 1]   mpg         Miles/(US) gallon"),
      p("  [, 2]	 cyl	 Number of cylinders"),
      p("  [, 3]	 disp	 Displacement (cu.in.)"),
      p("  [, 4]	 hp	 Gross horsepower"),
      p("  [, 5]	 drat	 Rear axle ratio"),
      p("  [, 6]	 wt	 Weight (lb/1000)"),
      p("  [, 7]	 qsec	 1/4 mile time"),
      p("  [, 8]	 vs	 V/S"),
      p("  [, 9]	 am	 Transmission (0 = automatic, 1 = manual)"),
      p("  [,10]	 gear	 Number of forward gears"),
      p("  [,11]	 carb	 Number of carburetors"),
      
      h3("Source"),
      
      p("Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.")
    )
  )
)
