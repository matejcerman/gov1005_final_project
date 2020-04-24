library(shiny)
library(shinythemes)

source('summary.R')

ui <- navbarPage("Performance of Slovak Schools",
    tabPanel('Overview'),
    tabPanel('Maps'),
    tabPanel('Visualizations'),
    tabPanel('Model'),
    tabPanel('About')
)

server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
