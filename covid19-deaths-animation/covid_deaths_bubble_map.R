library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(stringr)
library(waiter)
library(shinydashboard)
library(gganimate)
library(ggplot2)
library(png)
library(gifski)
library(shinycssloaders)

createRiseInCasesAnimation <- function(data_set) {
  confirmed_cases <-  data_set[-c(1,3,4)] # removing province, longitude and lattitude
  
  confirmed_cases  <- confirmed_cases %>% group_by(.data$`Country.Region`,.data$day) %>% summarise(cases = sum(cases))
  
  confirmed_cases <- confirmed_cases %>%
    group_by(day) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-cases),
           Value_lbl = paste0(" ",round(cases))) %>%
    group_by(.data$`Country.Region`) %>% arrange(desc(.data$cases)) %>% filter(.data$rank <11)
  
  staticplot = ggplot(confirmed_cases, aes(rank, group = `Country.Region`,
                                           fill = as.factor(`Country.Region`), color = as.factor(`Country.Region`))) +
    geom_tile(aes(y = cases/2,
                  height = cases,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(`Country.Region`, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=cases,label = Value_lbl, hjust=0)) +
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
         caption  = "COVID19 deaths cases | Data Source: John Hopkins")
  
  animate(anim, 100, fps = 4,  width = 500, height = 500, renderer = gifski_renderer("./cases.gif"))
}

createRecoveredCasesAnimation <- function(data_set) {
  recovered_cases <-  data_set[-c(1,3,4)] # removing province, longitude and lattitude
  
  recovered_cases  <- recovered_cases %>% group_by(.data$`Country.Region`,.data$day) %>% summarise(recovered = sum(recovered))
  
  recovered_cases <- recovered_cases %>%
    group_by(day) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-recovered),
           Value_lbl = paste0(" ",round(recovered))) %>%
    group_by(.data$`Country.Region`) %>% arrange(desc(.data$recovered)) %>% filter(.data$rank <11)
  
  staticplot = ggplot(recovered_cases, aes(rank, group = `Country.Region`,
                                           fill = as.factor(`Country.Region`), color = as.factor(`Country.Region`))) +
    geom_tile(aes(y = recovered/2,
                  height = recovered,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(`Country.Region`, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=recovered,label = Value_lbl, hjust=0)) +
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
         caption  = "COVID19 deaths cases | Data Source: John Hopkins")
  
  animate(anim, 100, fps = 4,  width = 500, height = 500, renderer = gifski_renderer("./recovered.gif"))
}

createDeathsAnimation <- function(data_set) {
  deaths_data <-  data_set[-c(1,3,4)] # removing province, longitude and lattitude
  
  deaths_data  <- deaths_data %>% group_by(.data$`Country.Region`,.data$day) %>% summarise(deaths =sum(deaths))
  
  ranked_deaths_data <- deaths_data %>%
    group_by(day) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(rank = rank(-deaths),
           Value_lbl = paste0(" ",round(deaths))) %>%
    group_by(.data$`Country.Region`) %>% arrange(desc(.data$deaths)) %>% filter(.data$rank <11)
  
  staticplot = ggplot(ranked_deaths_data, aes(rank, group = `Country.Region`,
                                              fill = as.factor(`Country.Region`), color = as.factor(`Country.Region`))) +
    geom_tile(aes(y = deaths/2,
                  height = deaths,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(`Country.Region`, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=deaths,label = Value_lbl, hjust=0)) +
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
         caption  = "COVID19 deaths cases | Data Source: John Hopkins")
  
  animate(anim, 100, fps = 4,  width = 500, height = 500, renderer = gifski_renderer("./deaths.gif"))
}

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
    menuItem("Dashboard", tabName = "dashboard", icon = icon("map")),
    menuItem("Charts", tabName = "charts", icon = icon("line-chart"))
  )
)

dashboardUi <- fluidPage(
  fluidRow(class = "text-center",
           valueBoxOutput("cases")
           ,valueBoxOutput("recovered")
           ,valueBoxOutput("deaths")), 
  fluidRow(
    use_waiter(),
    waiter_show_on_load(spinner),
    leafletOutput("map", height=1000)
  ))

chartsUi <- fluidPage(
  fluidRow(class = "text-center",
           valueBoxOutput("chartcases")
           ,valueBoxOutput("chartrecovered")
           ,valueBoxOutput("chartdeaths")
  ),
  fluidRow(
    column(width = 4,
           align = "center",
           br(),
           fluidRow(
             plotOutput("caseschart") %>% withSpinner(color="#518bb8")
           )
    ),
    column(width = 4,
           align = "center",
           br(),
           fluidRow(
             plotOutput("recoveredchart") %>% withSpinner(color="#518bb8")
           )
    ),
    column(width = 4,
           align = "center",
           br(),
           fluidRow(
             plotOutput("deathchart") %>% withSpinner(color="#518bb8")
           )
    )
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem("dashboard", dashboardUi),
    tabItem("charts", chartsUi)
  )
)

#completing the ui part with dashboardPage
ui <- tagList(
  dashboardPage(title = 'Corona Virus-19 Cases', header, sidebar, body, skin='blue'),
  tags$footer("Data source : https://github.com/CSSEGISandData/COVID-19", 
              align = "center", 
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:35px;
              color: white;
              padding: 8px;
              background-color: #518bb8;
              z-index: 1000;")
)

server <- function(input, output) {
  data_set <- loadData()
  selected_date = max(data_set$day)
  data_for_map <- data_set %>% filter(day == selected_date)
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
  
  output$chartcases <- renderValueBox({
    valueBox(
      tags$p('Total Cases : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$cases), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  
  output$chartrecovered <- renderValueBox({
    valueBox(
      tags$p('Total Recovered : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$recovered), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
  })
  
  output$chartdeaths <- renderValueBox({
    valueBox(
      tags$p('Total Deaths : ', style = "font-size: 100%;")
      ,tags$p(format(sum(data_for_map$deaths), big.mark=","), style = "font-size: 200%;")
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  })
  
  output$map <- renderLeaflet({
    waiter_hide()
    
    leaflet(data_for_map, options = leafletOptions(minZoom = 3, maxZoom = 5, worldCopyJump = FALSE, zoomControl = FALSE, maxBoundsViscocity = 1.0)) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      setView(lng = 79.08, lat = 21.14, zoom = 3) %>% #center at India and with initial zoom to 3
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data_for_map$Long, data_for_map$Lat,
                       fillColor = "#FF0000", fillOpacity = 0.5, color="white", radius= data_for_map$cases * 0.001 + 12, stroke=FALSE,
                       label = map_pop_up_text,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      )
  })
  
  output$deathchart <- renderImage({
    createDeathsAnimation(data_set)
    list(src = "deaths.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = FALSE)
  
  output$recoveredchart <- renderImage({
    createRecoveredCasesAnimation(data_set)
    list(src = "recovered.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = FALSE)
  
  output$caseschart <- renderImage({
    createRiseInCasesAnimation(data_set)
    list(src = "cases.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
