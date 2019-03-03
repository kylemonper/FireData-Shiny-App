library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(shinydashboard)
#library(fontawesome)



#####to do list#####
# must do:
    # - finish graphs
    # - fix layout: table below map, valueOutputs and graphs on side https://rstudio.github.io/shinydashboard/structure.html
    # - Fill out the 'about sectiion' and make it its own page

#cool to do
    # - remove header and replace with image http://jonkatz2.github.io/2018/06/22/Image-In-Shinydashboard-Header
    # - make data table reactive 

#make data table reactive 



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



####### Sidebar
sidebar <- dashboardSidebar(
  ##slider for year selection
  sliderInput("date_range",               
              label = "Select Date", 
              min = min(top100$YEAR_), 
              max = max(top100$YEAR_),
              value = range(top100$YEAR_),
              step = 1,
              sep = "",
              width = 400),
  #select Causes
  selectInput(inputId = "cause",        
              label = "Cause of Fire", 
              choices = c(sort(unique(top100$CAUSE)),'All')),
  #side bar tabs: 
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", tabName = "dashboard", icon = icon("fab fa-info-circle",lib='font-awesome')),
    menuItem("Get Code", icon = icon("fab fa-github",lib='font-awesome'), 
             href = "https://github.com/kylemonper/FireData-Shiny-App")
  )
)



##### Designing the dashboard Body Layout
layout <- fluidRow(
  #first column, with map and table
        column(width = 8,
                box(background = "black",
                    width = 12,
                    leafletOutput("map", height = 700, width = 800)),
                box(width = 12,
                  dataTableOutput('dto', width = 800))),
      #second column with graphs and info boxes
        column(4,
          fluidRow(width = 4,
                   box(width = 12,
                       title = "Quick Stats", 
                       background = "blue",
                       solidHeader = TRUE,
            valueBoxOutput("count", width = 4),
            valueBoxOutput("acres", width = 8))),
          box(width = 12,
              background = "blue",
              title = "<b>Fire Causes</b>",
              plotOutput("causePlot"))
          
        ))

body <- dashboardBody(layout)




ui <- dashboardPage(header, sidebar, body)


##############################################################################
# Server Side
##############################################################################

server <- function(input, output, session) {
  
  ##############Reactive Variables###################
  
  #create new reactive df based on slider date inpute in the ui
  reactive_date <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2])
  })
  
  #Do the same ^ for the table, but drop the geometry for the sake of displaying rows
  table <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
      st_drop_geometry(.)
  })
  
  
  ########################cause plot###########################
  reactive_cause<- reactive({
    if(input$cause == 'All') 
    {top100 %>% 
        group_by(YEAR_) %>%
        summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
        mutate(acres_burn_tot_1000 = acres_burn_tot/1000) 
    }
    
    else {top100 %>%
        filter(CAUSE == input$cause) %>% 
        group_by(YEAR_) %>%
        summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
        mutate(acres_burn_tot_1000 = acres_burn_tot/1000)
    }})
  
  
  #Make plot based on cause
  output$causePlot <- renderPlot({
    
    # draw the plot with the specified cause
    ggplot(data = reactive_cause(), aes(x = YEAR_, y = acres_burn_tot_1000))+
      geom_col(fill = "firebrick1", colour = "firebrick4")+
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limit = c(1877,2017))+
      scale_y_continuous(expand = c(0,0), limit = c(0, 510))+
      labs(y = "Fire Size (Thousands of Acres)", x = "Year")
  })


  
############################Value Boxes######################
  
  #valuebox 1: count of fires within the selection range
  output$count <- renderValueBox(
    valueBox(
      paste0(nrow(reactive_date())), 
      "Total Count", 
      color = "red"
    )
  )
  
  #valuebox 2: summing acres withing selection range
  output$acres <- renderValueBox(
    valueBox(
      paste0(sum(reactive_date()$GIS_ACRES)), 
      "Total Acres Burned", 
      color = "red", 
      icon = icon("fas fa-fire",lib='font-awesome')
    )
  )
  
  
  
  #######################maps and data table##################
  
  
  # Data table
  output$dto <- renderDT({
    datatable(table(), 
              rownames=F, 
              extensions = "Scroller", 
              width = "100%", 
              style="bootstrap",
              selection = "single",
              options = list(deferRender = TRUE, scrollY = 300,scrollX=FALSE, scroller = TRUE, stateSave = TRUE))
  })
  


  #this outputs the map
  output$map <- renderLeaflet({
    #static background map
    leaflet(top100) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>% 
      addPolygons(
        popup = paste("<h5 style = 'color: red'> Fire Description </h5>", 
                      "<b>Fire name:</b>", top100$FIRE_NAME, "<br>", 
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
                      "<b>Fire name:</b>", reactive_date()$FIRE_NAME, "<br>", 
                      "<b> Year: </b>", reactive_date()$YEAR_,"<br>", 
                      "<b>Size:</b>", reactive_date()$GIS_ACRES, "Sq.Acres", "<br>", 
                      "<b>Cause code</b>", reactive_date()$CAUSE, 
                      sep = " ")
        
      ) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

