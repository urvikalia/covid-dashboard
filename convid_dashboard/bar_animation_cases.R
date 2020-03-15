## Bar animation for corona virus cases 
library(ggplot2)
library(gganimate)
library(tidyverse)
library(jaiator)
library(scales)
library(gifski)

confirmed_dataset <-dplyr::as_tibble(data.table::fread("./data/time_series_19-covid-Confirmed.csv"))
confirmed_dataset <-  confirmed_dataset[-c(1,3,4)] # removing province, longitude and lattitude



## converting from wide to long data form
confirmed_data <-confirmed_dataset %>%
  pivot_longer(cols =c(2:ncol(confirmed_dataset)), values_to = "confirmed_cases", names_to = "date" )  %>% 
  mutate(date = as.Date(date, format="%m/%d/%y"))


confirmed_data  <- confirmed_data %>% group_by(.data$`Country/Region`,.data$date) %>% summarise(total_confirmed_cases =sum(confirmed_cases))
#confirmed_data  <- confirmed_data %>% group_by(.data$`Country/Region`) %>% summarise(cases =sum(total_confirmed_cases))

#confirmed_data %>% arrange(desc(.data$total_confirmed_cases))


confirmed_data_formatted <- confirmed_data %>%
  group_by(date) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-total_confirmed_cases),
         Value_lbl = paste0(" ",round(total_confirmed_cases))) %>%
  group_by(.data$`Country/Region`) %>% arrange(desc(.data$total_confirmed_cases)) %>% filter(.data$rank <11)

  
staticplot = ggplot(confirmed_data_formatted, aes(rank, group = .data$`Country/Region`, 
                                       fill = as.factor(.data$`Country/Region`), color = as.factor(.data$`Country/Region`))) +
  geom_tile(aes(y = total_confirmed_cases/2,
                height = total_confirmed_cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(.data$`Country/Region`, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=total_confirmed_cases,label = Value_lbl, hjust=0)) +
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


anim = staticplot + transition_states(date, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE,fixed_y = TRUE)  +
  ease_aes('sine-in-out')+
  labs(title = 'Corona Confirmed cases :{closest_state}',  
       #subtitle  =  "Top 10 Countries :{closest_state}" ,
       caption  = "Number of Confirmed cases")



animate(anim, 200, fps = 4,  width = 1200, height = 1000, renderer = ffmpeg_renderer(),rewind = TRUE) -> for_mp4


anim_save("animation.mp4", animation = for_mp4 )



### Seams like End of Feb there was a spike in number of cases for Sk, Iran and Itally 
## raw way to understand 

confirmed_first_case <- confirmed_data %>% group_by(.data$`Country/Region`) %>% filter(.data$total_confirmed_cases==1) %>% arrange() %>% slice(1)

confirmed_group_by_date <- confirmed_first_case %>% group_by(.data$date) %>% group_split()


## Indian Cases 
indian_confirmed_cases <- confirmed_data %>% filter(.data$`Country/Region`=="India") 

ggplot(data=indian_confirmed_cases, aes(x=date, y=total_confirmed_cases, group=1)) +geom_line(color="red")

