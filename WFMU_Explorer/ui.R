
library(shiny)
library(rmarkdown)
shinyUI(navbarPage("WFMU Explorer ALPHA VERSION",
                   # Application title
                   # tabPanel("Start Here",
                   #          mainPanel(
                   #            h4("Start by loading playlists.  This takes a minute but only needs to be done once."),
                   #            actionButton("load","Load Playlists")
                   #          )
                   # ),
                   tabPanel("Station",
                            titlePanel("Top Artists and Songs Played on WFMU"),
                            
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                selectInput("selection", "On Current Schedule:",
                                            choices = c('YES','NO','ALL')),
                                hr(),
                                sliderInput("years_back",
                                            "Years Back to Include:",
                                            min = 0,  max = 30, value = c(0,3)),
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
                   ),
                   tabPanel("DJs",
                            titlePanel("Nothing Here Yet")
                   ),
                   tabPanel("Artists",
                            titlePanel("Nothing Here Yet")
                   ),
                   tabPanel("Songs",
                            titlePanel("Nothing Here Yet")
                   ),
                   tabPanel("About",
                            mainPanel(
                              includeMarkdown("https://www.dropbox.com/s/f61zsbwos1jc6ga/about.md?dl=1")
                              )
                   )
                   
))
