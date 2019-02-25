library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(shinydashboard)
library(fontawesome)
##might be a cool thing to do with our graphs:
#app = system.file('examples', 'DT-rows', package = 'DT')
#runApp(app)



#####to do list:

#make data table reactive
#read in pdf & add data to working df
#change layout -- make map full length on side, with datatable under the widget on the side


##############################################################################
# Wrangling
##############################################################################


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
  )) 

#simplify polygons
top100 <- top100 %>% 
  st_simplify(dTolerance = 100)

#add centriods to df to be used in marker creation
centroid <- st_centroid(top100)
centroid_less <- centroid %>% 
  mutate(lat = unlist(map(centroid$geometry,1)),
         long = unlist(map(centroid$geometry,2))) %>% 
  select(GIS_ACRES, lat, long) %>% 
  st_drop_geometry()
top100 <- merge(top100, centroid_less, by = "GIS_ACRES") %>% 
  arrange(YEAR_)




#change projection to be compatible with leaflet
top100 <- st_transform(top100, crs = 4326)



##############################################################################
# UI
##############################################################################
header <- dashboardHeader(title = "Cal Fires")

sidebar <- dashboardSidebar(
  sliderInput("date_range", 
              label = "Select Date", 
              min = min(top100$YEAR_), 
              max = max(top100$YEAR_),
              value = range(top100$YEAR_),
              step = 1,
              sep = "",
              width = 400),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "dashboard", icon = icon("fab fa-info-circle",lib='font-awesome')),
    menuItem("Get Code", icon = icon("fab fa-github",lib='font-awesome'), 
             href = "https://github.com/kylemonper/FireData-Shiny-App")
  )
)


frow1 <- fluidRow(
  valueBoxOutput("count")
  ,valueBoxOutput("acres")
  ,valueBoxOutput("value3")
)

stuff <- column(width = 12,
                box(background = "black",
                    leafletOutput("map", height = 700, width = 900)),
                box(
                  dataTableOutput('dto', width = 900))
)






body <- dashboardBody(frow1, stuff)

ui <- dashboardPage(header, sidebar, body)







##############################################################################
# Server Side
##############################################################################

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
  
  
  #valuebox 1
  output$count <- renderValueBox(
    valueBox(
      paste0(nrow(reactive_date())), "Total Count", color = "red"
    )
  )
  
  output$acres <- renderValueBox(
    valueBox(
      paste0(sum(reactive_date()$GIS_ACRES)), "Total Area Burned", color = "red", icon = icon("fas fa-fire",lib='font-awesome')
    )
  )
  
  # Data table
  output$dto <- renderDT({
    datatable(table(), rownames=F, extensions = "Scroller", width = "100%", style="bootstrap", selection = "single",
              options = list(deferRender = TRUE, scrollY = 300,scrollX=FALSE, scroller = TRUE, stateSave = TRUE))
  })
  


  #this outputs the map
  output$map <- renderLeaflet({
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
                      "<b>Fire name:</b>", reactive_date()$FIRE_NAME, "<br", 
                      "<b> Year: </b>", reactive_date()$YEAR_,"<br>", 
                      "<b>Size:</b>", reactive_date()$GIS_ACRES, "Sq.Acres", "<br>", 
                      "<b>Cause code</b>", reactive_date()$CAUSE, 
                      sep = " ")
        
      ) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

