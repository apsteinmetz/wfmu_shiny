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

load("./playlists.rdata")
load('./djkey.rdata')
DJKey$DJ<-as.character(DJKey$DJ)
playlists<-playlists %>% 
  ungroup() %>% 
  mutate(artist_song=paste(ArtistToken,Title))

get_top_artists<-memoise(function(onAir,years_back) {
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
    filter(AirDate>Sys.Date()-(365*years_back[2])) %>%  #date range?
    filter(AirDate<Sys.Date()-(365*years_back[1])) %>%  #date range?
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
})

get_top_songs<-memoise(function(onAir,years_back) {
  if (onAir=='ALL') {
    DJ_set <-playlists
  } else {
    DJ_set <-DJKey %>% 
      filter(onSched==onAir) %>% #on Sched or off?
      select(DJ) %>%  
      left_join(playlists,by='DJ')
  }
  top_songs<-DJ_set %>% 
    filter(AirDate>Sys.Date()-(365*years_back[2])) %>%  #date range?
    filter(AirDate<Sys.Date()-(365*years_back[1])) %>%  #date range?
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count))
  top_songs
})



# Define server logic
shinyServer(function(input, output) {
  top_artists_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-get_top_artists(input$selection,input$years_back)
      })
    })
    return(ret_val)
  })
  top_songs_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-get_top_songs(input$selection,input$years_back)
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
  

})
