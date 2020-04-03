
library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny App",
    tabPanel("About"),
    tabPanel("Plot",

    # Application title
    titlePanel("Investigating Associations Between the Quality of Schools and Average Income in Slovak Counties"),

        # Show an image of the plot that is rendered on the server side
        mainPanel(
            imageOutput("plt")
        )
    )
)
    
    
    # Define server logic 
    
    server <- function(input, output) {
        
        # Using renderImage() with a list that specifies the filename and image
        # resolution
        
        output$plt <- renderImage({
            list(src = "regions_rating_plt.png",
                 width="800",
                 height="800")
            
            # Using deleteFile = FALSE because we are showing a permanent,
            # pre-generated image, not a temporary one
            
        }, deleteFile = FALSE)
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)