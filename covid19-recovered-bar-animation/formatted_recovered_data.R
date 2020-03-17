
library(tidyverse)
library(gganimate)

#filter our dataset to retain only the top 10 countries for each day
recovered_data_tidy <- read_csv("./data/recovered_data_tidy.csv")

recovered_data_tidy <- recovered_data_tidy %>% group_by(Country.Region, day) %>% summarise(value=sum(value))

recovered_data_formatted <- recovered_data_tidy %>%
  group_by(day) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  group_by(Country.Region) %>% 
  #select only top 10 countries for each  day
  filter(rank <=10) %>%
  ungroup()

write.csv(recovered_data_formatted, "./data/recovered_data_formatted.csv")


#execute static_plot.R