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
library(xts)


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

get_sim_index<-memoise(function(dj1,dj2) {
  DJ_sim<-dj_similarity_tidy %>% 
    filter(DJ1==dj1,DJ2==dj2) %>%
    pull(Similarity)
  DJ_sim
})

artists_in_common<-memoise(function(dj1,dj2){
  artists<-playlists %>% 
    filter(DJ %in% c(dj1,dj2)) %>% 
    group_by(DJ,ArtistToken) %>% 
    summarise(n=n()) %>%
    spread(DJ,n) %>% 
    mutate(sum_x=rowSums(.[2:ncol(.)],na.rm=TRUE)) %>% 
    mutate(sd_x=.[2:ncol(.)] %>% na.fill(0) %>% apply(1,sd)) %>% 
    mutate(FaveIndex=trunc(sum_x-1.8*sd_x)) %>% 
    #select(ArtistToken,sum,FaveIndex) %>% 
    top_n(10) %>% 
    select(-sum_x,-sd_x) %>% 
    arrange(desc(FaveIndex)) %>% 
    select(-FaveIndex)
  artists
})

songs_in_common<-memoise(function(dj1,dj2){
  songs<-playlists %>% 
    filter(DJ %in% c(dj1,dj2)) %>%
    mutate(Artist_Title=paste(Artist,Title)) %>% 
    group_by(DJ,Artist_Title) %>% 
    summarise(n=n()) %>%
    spread(DJ,n) %>% 
    mutate(sum_x=rowSums(.[2:ncol(.)],na.rm=TRUE)) %>% 
    mutate(sd_x=.[2:ncol(.)] %>% na.fill(0) %>% apply(1,sd)) %>% 
    mutate(FaveIndex=trunc((sum_x-1.8*sd_x)*10)) %>% #apply some arbitrary scaling
    #select(ArtistToken,sum,FaveIndex) %>% 
    top_n(10) %>% 
    select(-sum_x,-sd_x) %>% 
    arrange(desc(FaveIndex)) %>% 
    select(-FaveIndex)
  songs
})


# ----------------- STUFF FOR ARTIST TAB -----------------------------
play_count_by_DJ<-memoise(function(artist_token,years_range,threshold){
  pc<- playlists %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    mutate(DJ=as.character(DJ)) %>% 
    filter(ArtistToken==artist_token) %>% 
    mutate(AirDate=as.yearqtr(AirDate))  %>% 
    group_by(DJ,AirDate) %>% 
    summarise(Spins=n()) %>% 
    arrange(AirDate)
  
  pc1<- pc %>% 
    filter(Spins>=threshold)
  
  #lump together all DJ's who played the artist less than 'threshold' times
  pc2<- pc %>% 
    filter(Spins<threshold) %>% 
    group_by(AirDate) %>% 
    summarise(Spins=n()) %>% 
    mutate(ShowName='AllOther')
  
  pc<-pc1 %>% 
    left_join(DJKey,by='DJ') %>% 
    bind_rows(pc2) %>% 
    ungroup() %>% 
    select(AirDate,ShowName,Spins)
  return(pc)
})

top_songs_for_artist<-memoise(function(artist_token,years_range){
  ts<-playlists %>% 
    filter(ArtistToken==artist_token) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    group_by(Title) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))
  return(ts)
})


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
    colset<-RColorBrewer::brewer.pal(11,'Paired')
    chordDiagram(cdf,annotationTrack = c('grid','name'),grid.col = colset)
    
  })
  
  output$DJ_plot_sim_index <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_3) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    ggplot(dj_similarity_tidy,aes(Similarity))+
      geom_density()+
      geom_vline(xintercept = get_sim_index(dj1,dj2),color='blue')
  })
  
  output$DJ_table_common_songs <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_3) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    songs_in_common(dj1,dj2)
  })
  output$DJ_table_common_artists <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_3) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    artists_in_common(dj1,dj2)
  })
  
  #------------------------------ARTIST TAB-----------------------------------
  reactive_artists<-reactive({
    input$artist_update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-play_count_by_DJ(input$artist_selection,
                                  input$artist_years_range,
                                  input$artist_all_other)
      })
    })
    return(ret_val)
  })
  
  output$artist_history_plot <- renderPlot({
    artist_history<-reactive_artists()
    gg<-artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ShowName))+geom_col()
    gg<-gg+labs(title=paste("Number of",input$artist_selection,"plays every quarter by DJ"))
    gg<-gg+scale_x_continuous()
    gg
  })
  output$top_songs_for_artist<-renderTable({
    top_songs_for_artist(input$artist_selection,input$artist_years_range)
  })
  # ------------------ SONG TAB -----------------
  
  
})
