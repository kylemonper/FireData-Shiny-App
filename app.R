library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(tmap)
library(plotly)

#####to do list:

#IMPORTANT: figure out how to convery `YEAR_` from factor to numeric in a way that makes sense

#mutate $Shape_area from sq. m to acres sq.
#redefine cause codes from $CAUSE to actual definitions so that the cause can be clearly stated in the popup


#consider: changing from polygons to points - might render faster in the app
#####must figure out how to actually read in the fire_point files


#consider also: do we want to onl include say, the largest 1000 fires in history range?

#select top 100 fires 
top100 <- fire %>% 
  select(YEAR_, FIRE_NAME,Shape_Area, CAUSE) %>% 
  arrange(-Shape_Area)
top100 <- top100[1:100,]


#change projection to be compatible with leaflet
top100 <- st_transform(top100, crs = 4326)



ui <- fluidPage(
  titlePanel("California Fires"),
  sidebarLayout(position = "left",
    mainPanel(leafletOutput("map")),
    sidebarPanel( 
           
           # Date slider 
           sliderInput("date_range", 
                       label = "Select Date", 
                       min = min(as.numeric(paste(top100$YEAR_))), 
                       max = max(as.numeric(paste(top100$YEAR_))),
                       value = range(as.numeric(paste(top100$YEAR_))),
                       step = 1,
                       sep = ""),
           
    # search bar based on fire names
    tags$div(title = "Search by name of fire", #this creates an information box that displays the text when hovering over the widget 
             selectInput(inputId = "fire", 
                         label = "Fire Name", 
                         choices = sort(unique(top100$FIRE_NAME))))
    )
  )
)


server <- function(input, output, session) {

  #create new reactive df based on slider date inpute in the ui
  reactive_date <- reactive({
    top100 %>%
      filter(as.numeric(paste(YEAR_)) >= input$date_range[1] & as.numeric(paste(YEAR_)) <= input$date_range[2])
  })
  
  #this outputs the map
  output$map <- renderLeaflet({
    
    
      #allows user to highlight a polygon based on the selected date range
      pal <- colorFactor(
        palette = "Red",
        domain = input$date_range
      )
      
      
 
      
    #static background map
    leaflet(top100) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
      addPolygons(
        popup = paste("<h3 style = 'color: red'> Fire Description </h3>", "<b>Fire name:</b>", fire_year$FIRE_NAME, "<br", "<b>Year:</b>", fire_year$YEAR_,"<br>", "<b>Size:</b>", fire_year$Shape_Area, "Sq.Meters", "<br>", "<b>Cause code</b>", fire_year$CAUSE, sep = " ") 
      )
   
  })
  
  # reactive polgon map
  observe({
    leafletProxy("map", data = reactive_date()) %>%
      clearShapes() %>%
      addPolygons(
        popup = paste("<h3 style = 'color: red'> Fire Description </h3>", "<b>Fire name:</b>", top100$FIRE_NAME, "<br", "<b>Year:</b>", top100$YEAR_,"<br>", "<b>Size:</b>", top100$Shape_Area, "Sq.Meters", "<br>", "<b>Cause code</b>", top100$CAUSE, sep = " ")
      ) 
  })

  

}

# Run the application 
shinyApp(ui = ui, server = server)

