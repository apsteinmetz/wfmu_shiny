library(rmarkdown)
library(lubridate)
library(dplyr)

load('DJKey.RData')
load("playlists.Rdata")
#load(file=url("https://www.dropbox.com/s/zobdwfuc3x1p2h8/playlists.Rdata?dl=1")) #playlists
#load(file=url("https://www.dropbox.com/s/are6e2jx8djvkl4/DJKey.RData?dl=1")) #DJKey

#limit DJ list to DJs that are present in playlist file
DJKey<-DJKey %>% 
  mutate(DJ=as.character(DJ)) %>% 
  semi_join(playlists,by='DJ') %>% 
  arrange(ShowName)

playlists<-playlists %>% 
  ungroup() %>% 
  mutate(artist_song=paste(ArtistToken,Title))