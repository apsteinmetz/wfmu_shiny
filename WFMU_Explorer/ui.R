
library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Top Artists and Songs Played on WFMU"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "On Current Schedule:",
                  choices = c('YES','NO','ALL')),
      hr(),
      sliderInput("years_back",
                  "Years Back to Include:",
                  min = 0,  max = 30, value = c(0,5)),
      actionButton("update","Update")
    ),
    
    # Show Word Cloud
    mainPanel(
      fluidRow(
        h4('Top Artists'),
        tabsetPanel(type = "tabs",
                  tabPanel("Word Cloud", plotOutput("cloud")),
                  tabPanel("Table", tableOutput("table_artists"))
      )),
      fluidRow(
        h4('Top Songs'),
        tableOutput("table_songs")
      )
    )
  )
))
