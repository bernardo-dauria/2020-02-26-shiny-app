library(shiny)
library(tidyverse)
library(magrittr)
library(gapminder)

gapminder %<>% mutate_at(c("year", "country"), as.factor)

gapminder_years <- unique(gapminder$year)
gapminder_countries <- unique(gapminder$country) %>% str_sort()

headerRow <- div(
                selectInput("selYear", 
                         label="Select the Year", 
                         multiple = TRUE,
                         choices=gapminder_years),
                selectInput("selCountry", 
                            label="Select the Coutry", 
                            multiple = TRUE,
                            choices=gapminder_countries)
)

dataPanel <- tabPanel("Data",
                      tableOutput("dataTable")
             )

plotPanel <- tabPanel("Plot",
                      plotOutput("plotData"))

ui <- navbarPage("Shiny App",
                 dataPanel,
                 plotPanel,
                 header = headerRow
)

server <- function(input, output) {
  
  gapminder_filtered <- reactive(gapminder %>% filter(year %in% input$selYear, country %in% input$selCountry))
  
  output$dataTable <- renderTable({
    req(input$selYear)
    gapminder_filtered()
    })
  output$plotData <- renderPlot({
    req(input$selCountry)
    req(input$selYear)
    gapminder_filtered() %>%
      ggplot(aes(x=country, y=pop, fill=year)) +
      geom_bar(stat="identity", position=position_dodge())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)