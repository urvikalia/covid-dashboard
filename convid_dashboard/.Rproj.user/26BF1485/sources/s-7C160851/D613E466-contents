#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui =  dashboardPagePlus(
    header = dashboardHeaderPlus(
        title               = "Covid Dashboard", 
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "gears"
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(),
    rightsidebar = rightSidebar(
        rightSidebarTabContent(
            id = 1, title  = "Params", icon = "compass", active = TRUE,
            fluidRow(
                
                # COHORT
                uiOutput("cohort_manager_sidebar")
            )
        )
    ),
    title = "DashboardPage"
)


server = function(input, output) { }



# Run the application 
shinyApp(ui = ui, server = server)
