library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(stringr)
library(waiter)

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
  
    map_data <- loadData()
    map_pop_up_text <- createMapPopUpText(map_data)
    
    waiter_hide()
    
    leaflet(map_data, options = leafletOptions(minZoom = 3, maxZoom = 3, worldCopyJump = FALSE, zoomControl = FALSE, maxBoundsViscocity = 1.0)) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setView(lng = 79.08, lat = 21.14, zoom = 3) %>% #center at India and with initial zoom to 3
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(map_data$Long, map_data$Lat,
                       fillColor = "#FF0000", fillOpacity = 0.5, color="white", radius= map_data$cases * 0.001 + 10, stroke=FALSE,
                       label = map_pop_up_text,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
  })
  
  createMapPopUpText <- function(map_data) {
    map_text <- paste(
      "Cases : ", map_data$cases, "<br/>",
      "Recovered : ", map_data$recovered , "<br/>",
      "Deaths : ", map_data$deaths , "<br/>",
      "Active : ", map_data$cases - map_data$recovered - map_data$deaths, "<br/>",
      "Province/State : ", map_data$`Province.State`, "<br/>",
      "Country : ", map_data$`Country.Region`, sep="") %>%
      lapply(htmltools::HTML)
    return(map_text) 
  }
  
  loadData <- function() {
    #fetch confirmed cases data
    confirmed_data_url = RCurl::getURLContent("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
    confirmed_data_raw = textConnection(confirmed_data_url)
    confirmed_data <- data.table(read.csv(confirmed_data_raw))
    
    confirmed_data <- confirmed_data %>%
      pivot_longer(c(5:ncol(confirmed_data)), names_to = "day", values_to = "cases") %>%
      mutate(day = as.Date(sub(".", "", day), "%m.%d.%y"))
    
    selected_date = max(confirmed_data$day)
    
    #fetch deaths data
    deaths_data_url = RCurl::getURLContent("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
    deaths_data_raw = textConnection(deaths_data_url)
    deaths_data <- data.table(read.csv(deaths_data_raw))
    
    deaths_data <- deaths_data %>%
      pivot_longer(c(5:ncol(deaths_data)), names_to = "day", values_to = "deaths") %>%
      mutate(day = as.Date(sub(".", "", day), "%m.%d.%y"))
    
    #fetch recovered cases data
    recovered_data_url = RCurl::getURLContent("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
    recovered_data_raw = textConnection(recovered_data_url)
    recovered_data <- data.table(read.csv(recovered_data_raw))
    
    recovered_data <- recovered_data %>%
      pivot_longer(c(5:ncol(recovered_data)), names_to = "day", values_to = "recovered") %>%
      mutate(day = as.Date(sub(".", "", day), "%m.%d.%y"))
    
    #merge all three categories data in one set
    merged_data <- merge(confirmed_data, deaths_data, by=c('Province.State', 'Country.Region', 'Lat', 'Long', 'day'))
    merged_data <- merge(merged_data, recovered_data, by=c('Province.State', 'Country.Region', 'Lat', 'Long', 'day'))
    
    merged_data <- merged_data %>% filter(cases > 0, day == as.Date(selected_date, format = "%m/%d/%y"))
    
    return(merged_data)
  }
}

# Run the application
shinyApp(ui = ui, server = server)
