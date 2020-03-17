library(gganimate)
library(ggplot2)
library(tidyverse)
library(gifski)

#line graph plot
ggplot(data=indian_confirmed_cases, aes(x=date, y=total_confirmed_cases, group=1)) + geom_line(color="red")

  ggplot(data=recovered_data_tidy %>% filter(Country.Region == "India"), aes(x=day, y=value, group=1)) +geom_line(color="red")


staticplot = ggplot(recovered_data_formatted, aes(rank, group = Country.Region, 
                                       fill = as.factor(Country.Region), color = as.factor(Country.Region))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country.Region, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))


anim = staticplot + transition_states(day, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'COVID19 : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "COVID19 recovered cases | Data Source: John Hopkins")

animate(anim, 200, fps = 10,  width = 1200, height = 1000, renderer = gifski_renderer("gganim.gif"))

#animate(anim, 200, fps = 4,  width = 1200, height = 1000, renderer = ffmpeg_renderer(),rewind = TRUE) -> for_mp4

#anim_save("animation.mp4", animation = for_mp4 )

