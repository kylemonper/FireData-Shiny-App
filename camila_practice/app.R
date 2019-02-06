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
               
               tabPanel("This app visually displays..."),
               
               sidebarPanel(
                 imageOutput("josh_edelson")
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$josh_edelson <- renderImage({
    outfil <- tempfile(fileext = '.png')
    
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  }


# Run the application 
shinyApp(ui = ui, server = server)

