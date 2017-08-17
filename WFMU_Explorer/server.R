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

#load("./playlists.rdata")
#load('./djkey.rdata')
DJKey$DJ<-as.character(DJKey$DJ)
#DJKey<-DJKey %>% 
#  mutate(onMic=ifelse(onSched,'YES','NO')) %>% select(DJ,ShowName,onSched=onMic,showCount)

get_top_artists<-memoise(function(onAir,years_back) {
  top_artists<-DJKey %>% 
    filter(onSched==onAir) %>% #on Sched or off?
    select(DJ) %>% 
    left_join(playlists,by='DJ') %>%
    filter(AirDate>Sys.Date()-(365*years_back)) %>%  #date range?
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
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
})
