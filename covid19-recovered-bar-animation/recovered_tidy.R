#ORDER 1

library(tidyverse)
library(janitor)

recovered_data <- read.csv("./data/time_series_19-covid-Confirmed.csv")

#select required columns
recovered_data <- recovered_data %>% select(2, 5:ncol(recovered_data))

#clean data
recovered_data_tidy <- recovered_data %>% 
  #convert the 'each day' columns to individual rows
  pivot_longer(c(2:ncol(recovered_data)), names_to = "day") %>%
  #remove first character(X) from the each day value
  mutate(day = sub(".", "", day))

#export formatted data to another csv
write_csv(recovered_data_tidy,"./data/recovered_data_tidy.csv")

#execute formatted_recovered_data