library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(spData)
library(shinydashboard)
##might be a cool thing to do with our graphs:
#app = system.file('examples', 'DT-rows', package = 'DT')
#runApp(app)



#####to do list:

#make data table reactive
#read in pdf & add data to working df
#change layout -- make map full length on side, with datatable under the widget on the side


#select top 100 fires 
top100 <- fire %>% 
  select(YEAR_, FIRE_NAME,GIS_ACRES, CAUSE) %>% 
  arrange(-GIS_ACRES)

#fix years as numeric
top100$YEAR_ <- unfactor(top100$YEAR_)

#define cause codes
top100 <- top100[1:10,]

#simplify polygons
top100 <- top100 %>% 
  st_simplify(dTolerance = 100)

#change projection to be compatible with leaflet
top100 <- st_transform(top100, crs = 4326)

#add centriods to df to be used in marker creation
centroid <- st_centroid(top100)
centroid_less <- centroid %>% 
  mutate(lat = unlist(map(centroid$geometry,1)),
         long = unlist(map(centroid$geometry,2))) %>% 
  select(GIS_ACRES, lat, long) %>% 
  st_drop_geometry()

# add lat long caloumns to top100 by GIS_ACRES which is only unique identifier
top100 <- merge(top100, centroid_less, by = "GIS_ACRES") %>% 
  arrange(YEAR_)



##############################################################################
# UI
##############################################################################

ui <- fluidPage(
  sidebarLayout(position = "right",
                mainPanel(leafletOutput("map", height = 700, width = 700),
                          tags$hr(),
                          dataTableOutput('dto', width = 800),
                           img(src = "rstudio.png", height = 140, width = 400)),
                sidebarPanel( 
                  
                  # Date slider 
                  sliderInput("date_range", 
                              label = "Select Date", 
                              min = min(top100$YEAR_), 
                              max = max(top100$YEAR_),
                              value = range(top100$YEAR_),
                              step = 1,
                              sep = "",
                              width = 500),
                  tags$img(src='bren.png')
                )
                
  )
  
)


##############################################################################
# Server Side
##############################################################################

server <- function(input, output, session) {
  
  reactive_date <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
      arrange(FIRE_NAME)
  })
  
  table <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
      st_drop_geometry(.)
  })
  
  # Data table
  output$dto <- renderDataTable({
    datatable(table(), selection = "single", options = list(stateSave = TRUE))
  })
  
  output$map <- renderLeaflet({
    #static background map
    leaflet(top100) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
      addPolygons()
  })

  observe({
   
    leafletProxy("map", data = reactive_date()) %>%
      clearShapes() %>%
      addPolygons()
        
       
  })
 
  
  # to keep track of previously selected row
  my_icon = makeAwesomeIcon(icon = 'map-marker', markerColor = 'green', iconColor = 'white')
  
  observeEvent(input$dto_rows_selected, {
    row_selected = table()[input$dto_rows_selected,] 
    leafletProxy('map01') %>% 
      addAwesomeMarkers(lat = row_selected$lat,
                        lng = row_selected$long,
                        icon = my_icon)
})
  
}
# Run the application 
shinyApp(ui = ui, server = server)