library(shiny)

# Define UI for correlation application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Residuals volatility scaling"),
  
  sidebarPanel(
    
    selectInput("choice", "Choose a residual series:",
                choices = c("UTA", "DEXUS", "IFM", "AT")),
    
    numericInput("n", "Number of days per month", 21),
    sliderInput("conversion", "Conversion factor:",
               min = 0, max = 1, value = 0.43, step = 0.01),
    submitButton("Update")
  ),
  mainPanel(
    
    h4("Summary"),
    verbatimTextOutput("summary"),
    
    h4("Plot"),
    plotOutput("plot")

  )
  
))
