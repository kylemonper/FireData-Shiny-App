library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(spData)
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
top100 <- top100[1:100,] %>% 
  mutate(CAUSE = case_when(
    CAUSE == 1 ~ "Lightning",
    CAUSE == 2 ~ "Equipment Use",
    CAUSE == 4 ~ "Campfire",
    CAUSE == 5 ~ "Debris",
    CAUSE == 6 ~ "Railroad",
    CAUSE == 7 ~ "Arson",
    CAUSE == 9 ~ "Miscellaneous",
    CAUSE == 10 ~ "Vehicle",
    CAUSE == 11 ~ "Power Line",
    CAUSE == 14 ~ "Unknown/Unidentified"
  )) %>% 
  arrange(YEAR_)

#change projection to be compatible with leaflet
top100 <- st_transform(top100, crs = 4326)



ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("California Fires"),
  sidebarLayout(position = "right",
    mainPanel(leafletOutput("map", height = 700, width = 700)),
    sidebarPanel( 
           
           # Date slider 
           sliderInput("date_range", 
                       label = "Select Date", 
                       min = min(top100$YEAR_), 
                       max = max(top100$YEAR_),
                       value = range(top100$YEAR_),
                       step = 1,
                       sep = "",
                       width = 500)
           
    )
  ),
  tags$hr(),
  dataTableOutput('dto', width = 800)
)


server <- function(input, output, session) {

  #create new reactive df based on slider date inpute in the ui
  reactive_date <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2])
  })
 
  table <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
    st_drop_geometry(.)
  })
  
  
  output$dto <- renderDT({
    datatable(table(), rownames=F, filter="top", extensions = "Scroller", width = "100%", style="bootstrap",
              options = list(deferRender = TRUE, scrollY = 300,scrollX=FALSE, scroller = TRUE, dom = 'tp'))
  })
  

  #this outputs the map
  output$map <- renderLeaflet({
    
    
      #allows user to highlight a polygon based on the selected date range
      pal <- colorFactor(
        palette = "Red",
        domain = input$tableId_row_last_clicked
      )
      
      
 
      
    #static background map
    leaflet(top100) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
      addPolygons(
        popup = paste("<h5 style = 'color: red'> Fire Description </h5>", 
                      "<b>Fire name:</b>", top100$FIRE_NAME, "<br", 
                      "<b>Year:</b>", top100$YEAR_,"<br>", 
                      "<b>Size:</b>", top100$GIS_ACRES, "Sq.Acres", "<br>", 
                      "<b>Cause</b>", top100$CAUSE,
                      sep = " ")
      )
   
  })
  
  # reactive polgon map
  observe({
    leafletProxy("map", data = reactive_date()) %>%
      clearShapes() %>%
      addPolygons(
        popup = paste("<h5 style = 'color: red'> Fire Description </h5>", 
                      "<b>Fire name:</b>", top100$FIRE_NAME, "<br", 
                      "<b> Year: </b>", top100$YEAR_,"<br>", 
                      "<b>Size:</b>", top100$GIS_ACRES, "Sq.Acres", "<br>", 
                      "<b>Cause code</b>", top100$CAUSE, 
                      sep = " ")
        
      ) 
  })



}

# Run the application 
shinyApp(ui = ui, server = server)

