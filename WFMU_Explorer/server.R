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
library(igraph)
library(circlize)


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
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
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
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
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
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
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
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count))
  top_songs
})

get_similar_DJs<-memoise(function(dj) {
  similar_DJs<-dj_similarity_tidy %>% 
  filter(DJ1==dj) %>% 
  arrange(desc(Similarity)) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  rename(DJ=DJ2) %>% 
  select(DJ,Similarity) %>% 
  left_join(DJKey,by='DJ') %>%
  mutate(Similarity=paste0(trunc(Similarity*100),"%")) %>% 
  #add target dj to top of table so we see the 2-letter code for the chord chart
  full_join(filter(DJKey,DJ==dj)) %>% 
  select(ShowName,DJ,onSched,showCount,Similarity) 
    
similar_DJs
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
    get_similar_DJs(dj1)
  })
  output$DJ_chord <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% pull(DJ)
    # get similar djs but remove target dj or matrix stuff will break
    sim_DJs<-get_similar_DJs(dj1) %>% filter(DJ!=dj1) %>% pull(DJ)
    dj_mat<-dj_mat<-as.matrix(djdtm[c(sim_DJs,dj1),])
    adj_mat1 = dj_mat %*% t(dj_mat)
    # set zeros in diagonal
    diag(adj_mat1) = 0
    #change from dJ to show name
    #dimnames(adj_mat1)<-rep(list(Docs=filter(DJKey,DJ  %in% row.names(adj_mat1)) %>% pull(ShowName)),2)
    # create graph from adjacency matrix
    graph_artists1 = graph.adjacency(adj_mat1, mode="undirected", weighted=TRUE, diag=FALSE)
    # get edgelist 1
    edges1 = get.edgelist(graph_artists1)
    
    # arc widths based on graph_artists1
    w1 = E(graph_artists1)$weight
    lwds = w1/20000
    #chord diagrams
    cdf<-bind_cols(as_tibble(edges1),value=lwds)
    #reorder columns gets a better appearance
    #cdf<- cdf %>% select(V2,V1,value)
    #make show names compact
    #cdf$V2<-str_replace_all(cdf$V2,"[^A-Z^a-z^ ^0-9]","")
    #cdf$V1<-str_replace_all(cdf$V1,"[^A-Z^a-z^ ^0-9]","")
    #cdf<-cdf %>% lapply(str_replace_all,"The ","") %>% bind_rows
    #cdf<-cdf %>% lapply(str_trim) %>% bind_rows
    #cdf<-cdf %>% lapply(str_replace_all," ",'\n') %>% bind_rows
    #cdf$value <- as.numeric(cdf$value)
    colset<-RColorBrewer::brewer.pal(11,'Paired')
    chordDiagram(cdf,annotationTrack = c('grid','name'),grid.col = colset)
    
  })
  
  # ------------------ ARTIST TAB -----------------
  # ------------------ SONG TAB -----------------
  
  
})
