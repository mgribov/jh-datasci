library(shiny)

shinyUI(
  navbarPage("Predictive Text Using N-Grams",
            
    #main app
    tabPanel("Input",
      fluidPage(
        titlePanel("Controls"),
        sidebarLayout(
          sidebarPanel(
            selectInput("words", "Number Of Words To Return", choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5), selected = 5),
            textInput("phrase", "Your Input")
          ),
                          
          # prediction output
          mainPanel(
            h3(textOutput("Main Panel")),
            tabsetPanel(
              type = "tabs", 
              tabPanel("Predicted Text", verbatimTextOutput("pred"))
            )
          )
        )
      )
    )
  )
)
