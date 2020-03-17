library(shiny)

aggdata <- read_csv("./data/recovered_data_formatted.csv");

aggdata <- aggdata %>% select(2:4)

ui <- shinyUI(
  fluidPage(
    selectInput("Country","Choose the country",
                choices = unique(aggdata$Country.Region)),
    plotOutput("linePlot")
  )
)

server <- shinyServer(function(input, output, session) {
  output$linePlot <- renderPlot({
    ggplot(data=aggdata %>% filter(Country.Region == input$Country), aes(x=day, y=value, group=1)) +geom_line(color="black")
  })
})

shinyApp(ui = ui, server = server)