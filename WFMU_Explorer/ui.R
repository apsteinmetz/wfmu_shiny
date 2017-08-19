
library(shiny)
library(rmarkdown)
library(lubridate)
library(dplyr)


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
                                selectInput("selection", "Are the DJs On Current Schedule?:",
                                            choices = c('YES','NO','ALL')),
                                hr(),
                                sliderInput("years_range",
                                            "Year Range:",
                                            min = min_year,
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
                   navbarMenu("DJs",
                              tabPanel("DJ Profile",
                                       titlePanel("DJ Profiles"),
                                       sidebarLayout(
                                         # Sidebar with a slider and selection inputs
                                         sidebarPanel(
                                           selectInput("show_selection", "Show Name:",
                                                       choices = DJKey$ShowName,
                                                       selected = 'Teenage Wasteland'),
                                           hr(),
                                           uiOutput("DJ_date_slider"),
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
                              tabPanel("Find Similar DJs",
                                       titlePanel("DJ Profiles"),
                                       sidebarLayout(
                                         # Sidebar with a slider and selection inputs
                                         sidebarPanel(
                                           selectInput("show_selection_2", "Show Name:",
                                                       choices = DJKey$ShowName,
                                                       selected = 'Teenage Wasteland')
                                         ),
                                         
                                         # Show Word Cloud
                                         mainPanel(
                                           fluidRow(
                                             h4('Most Similar Shows'),
                                             tableOutput("DJ_table_similar")
                                             
                                             ),
                                           fluidRow(
                                             h4('Common Links')
                                           )
                                         )
                                       )
                                       ),
                              tabPanel("Compare to a specific DJ",
                                       titlePanel("Nothing Here Yet")
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
                              includeMarkdown("about.md")
                            )
                   )
                   
))
