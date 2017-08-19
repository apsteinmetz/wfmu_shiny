#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(memoise)
library(wordcloud)
library(dplyr)
library(rmarkdown)
library(tidyverse)
library(lubridate)


# ----------------- STUFF FOR STATION TAB -----------------------------
get_top_artists<-memoise(function(onAir,years_range) {
  if (onAir=='ALL') {
    DJ_set <-DJKey %>% 
      select(DJ)
  } else {
    DJ_set <-DJKey %>% 
      filter(onSched==onAir) %>% #on Sched or off?
      select(DJ) 
    
  }
  top_artists<-DJ_set %>% 
    left_join(playlists,by='DJ') %>%
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  #date range?
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  #date range?
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
})

get_top_songs<-memoise(function(onAir,years_range) {
  if (onAir=='ALL') {
    DJ_set <-playlists
  } else {
    DJ_set <-DJKey %>% 
      filter(onSched==onAir) %>% #on Sched or off?
      select(DJ) %>%  
      left_join(playlists,by='DJ')
  }
  top_songs<-DJ_set %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  #date range?
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  #date range?
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count))
  top_songs
})
# ----------------- STUFF FOR DJ TAB -----------------------------
get_top_artists_DJ<-memoise(function(dj,years_range) {
  top_artists<-playlists %>%
    ungroup() %>% 
    filter(DJ==dj) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  #date range?
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  #date range?
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
})

get_top_songs_DJ<-memoise(function(dj,years_range) {
  top_songs<-playlists %>% 
    ungroup() %>% 
    filter(DJ==dj) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  #date range?
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  #date range?
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count))
  top_songs
})


# ----------------- STUFF FOR ARTIST TAB -----------------------------
# ----------------- STUFF FOR SONG TAB -----------------------------


# --------------------------------------------------------------
# Define server logic
shinyServer(function(input, output) {
  # ------------------ STATION TAB -----------------
  top_artists_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-get_top_artists(input$selection,input$years_range)
      })
    })
    return(ret_val)
  })
  top_songs_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-get_top_songs(input$selection,input$years_range)
      })
    })
    return(ret_val)
  })
  
  output$cloud <- renderPlot({
    wordcloud_rep <- repeatable(wordcloud,seed=1234)
    top_artists<-top_artists_reactive() 
    scaleFactor=2
    wordcloud_rep(words = top_artists$ArtistToken, 
                  freq = top_artists$play_count^scaleFactor,
                  max.words=100, 
                  random.order=FALSE,rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"),
                  scale = c(4,.3))
  })
  output$table_artists <- renderTable({
    top_artists_reactive()
  })
  output$table_songs <- renderTable({
    top_songs_reactive()
  })
  # ------------------ DJ TAB -----------------
  output$DJ_date_slider <- renderUI({
    sliderInput("DJ_years_range",
                "Year Range:",
                min = filter(DJKey,ShowName==input$show_selection) %>% pull(FirstShow) %>% year(),
                max = filter(DJKey,ShowName==input$show_selection) %>% pull(LastShow) %>% year(),
                sep = "",
                value = c(min,max)
    )
    
  })
  
  top_artists_reactive_DJ<-reactive({
    input$DJ_update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        DJ<-filter(DJKey,ShowName==input$show_selection) %>% pull(DJ)
        if (is.null(input$DJ_years_range)) {
          years_range<-c(1982,year(Sys.Date()))
        } else{
          years_range <- input$DJ_years_range
        }
        ret_val<-get_top_artists_DJ(DJ,years_range)
      })
    })
    return(ret_val)
  })
  top_songs_reactive_DJ<-reactive({
    input$DJ_update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        DJ<-filter(DJKey,ShowName==input$show_selection) %>% pull(DJ)
        if (is.null(input$DJ_years_range)) {
          years_range<-c(1982,year(Sys.Date()))
        } else{
          years_range <- input$DJ_years_range
        }
        ret_val<-get_top_songs_DJ(DJ,years_range)
      })
    })
    return(ret_val)
  })
  
  output$DJ_cloud <- renderPlot({
    wordcloud_rep <- repeatable(wordcloud,seed=1234)
    top_artists<-top_artists_reactive_DJ() 
    scaleFactor=2
    wordcloud_rep(words = top_artists$ArtistToken, 
                  freq = top_artists$play_count^scaleFactor,
                  max.words=100, 
                  random.order=FALSE,rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"),
                  scale = c(4,.3))
  })
  output$DJ_table_artists <- renderTable({
    top_artists_reactive_DJ()
  })
  output$DJ_table_songs <- renderTable({
    top_songs_reactive_DJ()
  })
  
  output$DJ_table_similar <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% pull(DJ)
    similar_DJS<-dj_similarity_tidy %>% 
      filter(DJ1==dj1) %>% 
      arrange(desc(Similarity)) %>% 
      top_n(10) %>% pull(DJ2)
    DJKey %>% filter(DJ %in% similar_DJS)
  })
  
  # ------------------ ARTIST TAB -----------------
  # ------------------ SONG TAB -----------------
  
  
})
