library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(spData)
library(shinydashboard)


#select top 100 fires 
top100 <- fire %>% 
  select(YEAR_, FIRE_NAME,GIS_ACRES, CAUSE) %>% 
  arrange(-GIS_ACRES) %>% 
  st_drop_geometry()

#fix years as numeric
top100$YEAR_ <- unfactor(top100$YEAR_)

#define cause codes
top100 <- top100[1:10,]





##############################################################################
# UI
##############################################################################

ui <- fluidPage(
  sidebarLayout(
                mainPanel(
                  plotOutput("causePlot")),
                           
                 sidebarPanel( 
                  uiOutput("date"),
                  uiOutput("cause")
                
                  
                )
                
  )
  
)



##############################################################################
# Server Side
##############################################################################

server <- function(input, output, session) {
  
  output$date <- renderUI({
    sliderInput("date_range", 
                label = "Select Date", 
                min = min(top100$YEAR_), 
                max = max(top100$YEAR_),
                value = range(top100$YEAR_),
                step = 1,
                sep = "",
                width = 500)
  })
  
  output$cause <- renderUI({
    selectInput(inputId = "cause",        
                label = "Cause of Fire", 
                choices = c(sort(unique(top100$CAUSE))))
  })
  
  
  
  reactive_all <- reactive({
    
    req(input$cause)
    req(input$date)
 top100 %>% 
   filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2] & CAUSE == input$CAUSE)
    

  })
  
  
  
  ########################cause plot###########################
  
  
  
  # Data table
  output$dto <- renderDataTable({
    datatable(reactive_all())
  })
  
       
  
 
  
  # to keep track of previously selected row
 # my_icon = makeAwesomeIcon(icon = 'map-marker', markerColor = 'green', iconColor = 'white')
  
 # observeEvent(input$dto_rows_selected, {
 #   row_selected <- table()[input$dto_rows_selected,] 
  #  leafletProxy('map01') %>% 
  #    addAwesomeMarkers(lat = row_selected$lat,
  #                      lng = row_selected$long,
  #                      icon = my_icon)
#})
  
}
# Run the application 
shinyApp(ui = ui, server = server)