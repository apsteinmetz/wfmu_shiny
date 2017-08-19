
library(shiny)
library(rmarkdown)
library(lubridate)

load('DJKey.RData')

shinyUI(navbarPage("WFMU Playlist Explorer ALPHA VERSION",
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
                                sliderInput("years_range",
                                            "Year Range:",
                                            min = 1982,
                                            max = year(Sys.Date()),
                                            sep = "",
                                            value = c(year(Sys.Date())-3,year(Sys.Date()))),
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
                            titlePanel("DJ Profiles"),
                            sidebarLayout(
                              # Sidebar with a slider and selection inputs
                              sidebarPanel(
                                selectInput("show_selection", "DJ:",
                                            choices = DJKey$ShowName),
                                hr(),
                                sliderInput("DJ_years_range",
                                            "Year Range:",
                                            min = 1982,
                                            max = year(Sys.Date()),
                                            sep = "",
                                            value = c(1982,year(Sys.Date()))),
                                actionButton("DJ_update","Update")
                              ),
                              
                              # Show Word Cloud
                              mainPanel(
                                fluidRow(
                                  h4('Top Artists'),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Word Cloud", plotOutput("DJ_cloud")),
                                              tabPanel("Table", tableOutput("DJ_table_artists"))
                                  )),
                                fluidRow(
                                  h4('Top Songs'),
                                  tableOutput("DJ_table_songs")
                                )
                              )
                            )
                            
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
