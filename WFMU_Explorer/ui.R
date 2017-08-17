
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Top Artists on WFMU"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "On Current Schedule:",
                  choices = c('YES','NO')),
      hr(),
      sliderInput("years_back",
                  "Years Back to Include:",
                  min = 1,  max = 25, value = 5),
      actionButton("update","Update")
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("cloud")
    )
  )
))
