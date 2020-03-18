library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(stringr)
library(waiter)

setwd("../") #updated working dir to fetch data at parent level

spinner <- tagList(
  spin_fading_circles(),
  h3("Loading...", style="color:white;")
)

ui <- fluidPage(
  fluidRow(
    use_waiter(),
    waiter_show_on_load(spinner),
    leafletOutput("map", height=1000)
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    fetchLatestData()

    retValue <- loadData()
    merged_data <- retValue[[1]]
    map_text <- retValue[[2]]
    
    hide_waiter()
    
    leaflet(merged_data, options = leafletOptions(minZoom = 3, maxZoom = 3, worldCopyJump = FALSE, zoomControl = FALSE, maxBoundsViscocity = 1.0)) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setView(lng = 79.08, lat = 21.14, zoom = 3) %>% #center at India and with initial zoom to 3
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(merged_data$Long, merged_data$Lat,
                       fillColor = "#FF0000", fillOpacity = 0.5, color="white", radius= merged_data$cases * 0.001 + 10, stroke=FALSE,
                       label = map_text,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
  })
  
  fetchLatestData <- function() {
    if(!dir.exists("./Covid-19")){
      system("git clone https://github.com/CSSEGISandData/COVID-19.git")
    } else {
      system("cd Covid-19/")
      system("git pull origin master")
    }
  }
  
  loadData <- function() {
    data_root_path <- "./COVID-19/csse_covid_19_data/csse_covid_19_time_series"
    
    confirmed_data <- read_csv(paste0(data_root_path, "/time_series_19-covid-Confirmed.csv"))
    confirmed_data <- confirmed_data %>% gather(day, cases, '1/22/20':ncol(confirmed_data))
    confirmed_data <- confirmed_data %>% mutate(day = as.Date(day, format = "%m/%d/%y"))
    
    selected_date <- max(confirmed_data$day)
    
    deaths_data <- read_csv(paste0(data_root_path, "/time_series_19-covid-Deaths.csv"))
    deaths_data <- deaths_data %>% gather(day, deaths, '1/22/20':ncol(deaths_data))
    deaths_data <- deaths_data %>% mutate(day = as.Date(day, format = "%m/%d/%y"))
    
    recovered_data <- read_csv(paste0(data_root_path, "/time_series_19-covid-Recovered.csv"))
    recovered_data <- recovered_data %>% gather(day, recovered, '1/22/20':ncol(recovered_data))
    recovered_data <- recovered_data %>% mutate(day = as.Date(day, format = "%m/%d/%y"))
    
    merged_data <- merge(confirmed_data, deaths_data, by=c('Province/State', 'Country/Region', 'Lat', 'Long', 'day'))
    merged_data <- merge(merged_data, recovered_data, by=c('Province/State', 'Country/Region', 'Lat', 'Long', 'day'))
    
    merged_data <- merged_data %>% filter(cases > 0, day == as.Date(selected_date, format = "%m/%d/%y"))
    # merged_data <- merged_data %>% filter(str_detect(`Country/Region`, "China"))
    
    map_text <- paste(
      "Cases : ", merged_data$cases, "<br/>",
      "Recovered : ", merged_data$recovered , "<br/>",
      "Deaths : ", merged_data$deaths , "<br/>",
      "Active : ", merged_data$cases - merged_data$recovered - merged_data$deaths, "<br/>",
      "Province/State : ", merged_data$`Province/State`, "<br/>",
      "Country : ", merged_data$`Country/Region`, sep="") %>%
      lapply(htmltools::HTML)
    
    return(list(merged_data, map_text))
  }
}

# Run the application
shinyApp(ui = ui, server = server)
