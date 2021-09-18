library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
library(tidyverse)
library(dplyr)
library(patchwork)
tuesdata$billboard->billboard
tuesdata$audio_features->audio
library(cowplot)
library(ggrepel)

glimpse(billboard)
glimpse(audio)
billboard%>%left_join(audio, by="song_id")->song
glimpse(song)
lubridate::mdy(song$week_id)->song$week_id

song%>%filter(peak_position==1)%>%select(song_id,song.x, peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->song1
song1[1:25,]->song2
song2$song.x= with(song2, reorder(song.x, weeks_on_chart, decreasing=TRUE))


song2%>%
ggplot(aes(x=weeks_on_chart, y=song.x))+
  geom_segment( aes(x=0, xend=weeks_on_chart, y=song.x, yend=song.x), color="#EF8C86")+
  geom_point(size = 7, colour="#FCB677") +
  geom_text(aes(label=weeks_on_chart),color = "black", size = 3)+
  theme(
    panel.background = element_rect(fill="black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face="bold", color="#FCB677"),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill="black"))+
    labs(title="BILLBOARD TOP 25", 
         subtitle = "Top 25 songs that stayed in the #1 spot for the highest number of weeks")+
    theme(plot.title = element_text(hjust = 0.4, size = 50, colour="#FCB677", face = "bold"),
          plot.subtitle = element_text(hjust= 0.3, size = 20, colour = "#FCB677", face="bold" ))->P1
  

song2%>%left_join(audio,by="song_id")->song3
distinct(song3, song_id, .keep_all = TRUE)->song3
song3%>%select(song.x,danceability, energy)->song4
song4 %>% drop_na()->song4
colnames(song4)<-c("Song","Danceability","Energy")

ggplot(song4, aes(x=Danceability, y=Energy, label=Song))+
  geom_point(color = "#EF8686", shape = "diamond", size = 4)+ 
  geom_label_repel(fill = "black", fontface="bold", color= "#EF8686", size=4, max.overlaps = Inf)+
  theme(
    panel.background = element_rect(fill="black"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face="bold",vjust=0,size=12, color="#FCB677"),
    axis.text.x = element_text(face="bold",vjust=0,size=12, color="#FCB677"),
    axis.title.x = element_text(face="bold", size=12,color="#FCB677"),
    axis.title.y = element_text(face="bold", size=12,color="#FCB677"),
    axis.line = element_line(colour="#685268"),
    plot.background = element_rect(fill="black"))+
  labs(title="HOW LIVELY ARE THE SONGS?", 
       subtitle = "Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. 
       A value of 0.0 is least danceable and 1.0 is most danceable. 
       Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.",
       caption = "Data: Data.World via Sean Miller|Design and Analysis: @annapurani93")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, colour="#FCB677", face = "bold"),
        plot.subtitle = element_text(hjust= 0.5, size = 12, colour = "#FCB677",face="bold"),
        plot.caption =  element_text(size = 12, colour = "#FCB677", face="bold"))->P2


P1/P2->P3
P3
ggsave("P3.png",P1, width=40, height=30, units="cm")
