#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Convid Dashboard",
                                    shinydashboard::dropdownMenuOutput("messageMenu")),
    shinydashboard::dashboardSidebar(
        width = 150,
        shinydashboard::sidebarUserPanel(""),
        shinydashboard::sidebarMenu(
            id = "tabs",
            shinydashboard::menuItem("Cases", tabName = "cases", icon = icon("archive")),
            shinydashboard::menuItem("Deaths", tabName = "deaths", icon = icon("archive")),
            shinydashboard::menuItem("Recoveries", tabName = "recovery", icon = icon("archive"))
        )
    ),
    ## Body content
    shinydashboard::dashboardBody(
        tags$style(
            HTML(
                "
      .box.box-solid.box-info>.box-header {
      color:#fff;
      background:#666666}
      .box.box-solid.box-info{
      border-bottom-color:#666666;
      border-left-color:#666666;
      border-right-color:#666666;
      border-top-color:#666666;}"
            )
        ),
        
        shinydashboard::tabItems(
            shinydashboard::tabItem(
                tabName = "cases",
                h2("Confirmed cases"),
                h4("World wide distribution"),
                fluidRow(
                    shinydashboard::tabBox(
                        id="cases_tab_box",
                        width = 12,
                        side = "left",
                        selected = "settings",
                        tabPanel(
                            "Load raw data",
                            fluidRow(
                                shinydashboard::box(
                                    id ="etlUploadSection",
                                    width = 12,
                                    title = "Upload Files",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    fluidRow()
                                    )
                                )
                            )
                )
            )
            )
        
        )## tabitem closing
        
        
    )## end of dashboard body
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
