#scratchpad
playlists %>% 
  filter(ArtistToken %in% c("JustinBieber","Rihanna","CharlesManson")) %>% 
  left_join(DJKey) %>%
  mutate(AirDate=year(AirDate)) %>% 
  group_by(AirDate, ArtistToken) %>% 
  summarize(PlayCount=n()) %>% 
    ggplot(aes(AirDate,PlayCount,fill=ArtistToken))+
  geom_col()+
  scale_y_continuous(breaks = seq(0,18,by=2))+
  theme_economist()

temp<-playlists %>%
  group_by(ArtistToken) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

gg<-playlists %>%
  filter(ArtistToken %in% c("Stooges","Voivod","NinaSimone")) %>% 
  left_join(DJKey) %>%
  mutate(AirDate=year(AirDate)) %>% 
  filter(AirDate>2005) %>%
  mutate(AirDate=factor(AirDate)) %>% 
    group_by(AirDate,ArtistToken) %>% 
  summarize(PlayCount=n()) %>% 
  arrange(desc(PlayCount)) %>% 
  ggplot(aes(AirDate,PlayCount,fill=ArtistToken))+geom_col()

gg