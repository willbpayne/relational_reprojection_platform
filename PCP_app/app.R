#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dataFile <- "IND_remittances.csv"

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pseudo-Spatial Chart Program"),
   
   ##### NB: WIDGET: Close/med/far text entry
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     # sidebarPanel(
     #   fileInput("inputId", "label", multiple = FALSE, accept = NULL)
     #              ),
      
     sidebarPanel(
       fileInput("uploadedFile", "Data File", multiple = FALSE, accept = NULL
       ),  
       # radio buttons
       radioButtons("interpMeth", "interpolation method", 
                    choices = c("Square Root", "Logarithmic", "Exponential", "Custom"), 
                    inline = TRUE, width = "100%"),
       ## if radio button custom
    #   conditionalPanel(condition, ...
       sliderInput("manualCutPoints", 
                   "cut points", 0, 500, #to be parametarized
                   "value", step = NULL, 
                   # value is 1 if it is a single value or 2 if it is a vector of 2 eg, [x,y]
                   round = FALSE, 
                   format = "#,##0.#####", 
                   locale = "us", 
                   ticks = TRUE, animate = FALSE) #,
   #  ) # end conditional panel
     ),
      ##### NB: WIDGET: Add second side bar for dist type dropdown
      
      # Show a plot of the generated distribution
      mainPanel(
       
         ##### NB: This should be our map instead
         plotOutput("distPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### NB: Function-ize map data and put it here 
  
   output$distPlot <- renderPlot({
     df <- read.csv(file = dataFile)
      plot(df$lon, df$lat)
      
      if uploadFile = !NULL,
        something something something

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

