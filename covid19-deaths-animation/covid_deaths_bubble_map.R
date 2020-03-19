library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(stringr)
library(waiter)
library(shinydashboard)
library(gganimate)
library(ggplot2)

createMapPopUpText <- function(data_set) {
  map_text <- paste(
    "Cases : ", data_set$cases, "<br/>",
    "Recovered : ", data_set$recovered , "<br/>",
    "Deaths : ", data_set$deaths , "<br/>",
    "Active : ", data_set$cases - data_set$recovered - data_set$deaths, "<br/>",
    "Province/State : ", data_set$`Province.State`, "<br/>",
    "Country : ", data_set$`Country.Region`, sep="") %>%
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
  
  merged_data <- merged_data %>% filter(cases > 0)
  
  return(merged_data)
}

spinner <- tagList(
  spin_fading_circles(),
  h3("Loading...", style="color:white;")
)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Corona Virus-19 Cases")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("map"))
  )
)

frow1 <- shinydashboard::box(
  width = 12,
  title = tags$p('Cases : ', style = "font-size: 120%; padding-left:5px"),
  solidHeader = FALSE,
  collapsible = TRUE,
  status = "warning",
  fluidRow(class = "text-center",
           valueBoxOutput("cases")
           ,valueBoxOutput("recovered")
           ,valueBoxOutput("deaths")
  )
)



frow2 <- fluidRow(
  use_waiter(),
  waiter_show_on_load(spinner),
  leafletOutput("map", height=1000)
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem("dashboard", frow1, frow2)
  )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Corona Virus-19 Cases', header, sidebar, body, skin='blue')

server <- function(input, output) {
  data_set <- loadData()
  selected_date = max(data_set$day)
  data_for_map <- data_set %>% filter(day == as.Date(selected_date, format = "%m/%d/%y"))
  map_pop_up_text <- createMapPopUpText(data_for_map)
  
  output$cases <- renderValueBox({
    valueBox(
      tags$p('Total Cases : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$cases), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  
  output$recovered <- renderValueBox({
    valueBox(
      tags$p('Total Recovered : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$recovered), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
  })
  
  output$deaths <- renderValueBox({
    valueBox(
      tags$p('Total Deaths : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$deaths), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  })
  
  output$map <- renderLeaflet({
    waiter_hide()
    
    leaflet(data_for_map, options = leafletOptions(minZoom = 3, maxZoom = 3, worldCopyJump = FALSE, zoomControl = FALSE, maxBoundsViscocity = 1.0)) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setView(lng = 79.08, lat = 21.14, zoom = 3) %>% #center at India and with initial zoom to 3
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data_for_map$Long, data_for_map$Lat,
                       fillColor = "#FF0000", fillOpacity = 0.5, color="white", radius= data_for_map$cases * 0.001 + 12, stroke=FALSE,
                       label = map_pop_up_text,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
