library(shiny)
library(tidyverse)
library(shinythemes)
library(jpeg)

josh_edelson <- "Josh_Edelson_AFT_Getty.png"

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"),

   
   # Application title
   titlePanel("Playing with Fire... Data"),
    
    navbarPage("Summary",
               
               sidebarPanel(title=div(img(src="image1.png"), "My Title")
               ),
               tabPanel("Map"),
               tabPanel("Graphs")
                        # Sidebar with a slider input for number of bins 
                       # sidebarLayout(
                        #  sidebarPanel(
                          #  selectInput(inputId = "acres_burned",
                                      #  label = "Acres Burned", 
                                      #  choices = sort(unique(top100$CAUSE)))),
                        #  mainPanel(
                        #    plotOutput("acresPlot")
                          )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) { # Added session to see if it would help render the image
  output$Image <- renderImage({
    filename <- normalizePath(file.path('./www', 
                              paste('image', input$n, '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  }


# Run the application 
shinyApp(ui, server)

