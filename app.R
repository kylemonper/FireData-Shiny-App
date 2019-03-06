library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(shinydashboard)


#library(fontawesome)


#testing github
#####to do list#####
# must do:
# - Pie chart - aesthetic, date slider (keep brainstorming) CAMILA (JENNY help brainstorm?)
# - Fill out the 'about section' and make it its own page - CAMILA/JENNY FINISH

#cool to do
# - make data table reactive - KYLE
#set default of causeplot to "All", make it reactive with map - JENNY with KYLE help?





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

#ecoregion wrangling
eco <- st_read("ca_eco.shp") %>% 
  dplyr::select(US_L3NAME) %>% 
  rename(Region = US_L3NAME) %>% 
  st_simplify(dTolerance = 100) %>% # Simplify polygons so maps don't take forever to load
  st_transform(crs = 4326)

eco_intersect <- eco %>% 
  st_intersection(top100)

eco_pie_df <- st_set_geometry(eco_intersect, NULL)

eco_pie <- data.frame("Categorie"=rownames(eco_pie_df), eco_pie_df)

##############################################################################
# UI
##############################################################################
header <- dashboardHeader(title = "Playing With Fire...Data", titleWidth = 250)



####### Sidebar
sidebar <- dashboardSidebar(
  #side bar tabs: 
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("fab fa-info-circle",lib='font-awesome')),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
    menuItem("Get Code", icon = icon("fab fa-github",lib='font-awesome'), 
             href = "https://github.com/kylemonper/FireData-Shiny-App"),
    id = "tabs"
  ),
  #slider for year selection
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
  #eco checkbox
  checkboxGroupInput("checkRegion",
                     label = "Select Eco-Region",
                     choices = list("Cascades" = "Cascades",
                                    "Central Basin and Range" = "Central Basin and Range",
                                    "Central California Foothills and Coastal Mountains" = "Central California Foothills and Coastal Mountains",
                                    "Central California Valley" = "Central California Valley",
                                    "Eastern Cascades Slopes and Foothills" = "Eastern Cascades Slopes and Foothills",
                                    "Klamath Mountains/California High North Coast Range" = "Klamath Mountains/California High North Coast Range",
                                    "Mojave Basin and Range" = "Mojave Basin and Range",
                                    "Northern Basin and Range" = "Northern Basin and Range",
                                    "Sierra Nevada" = "Sierra Nevada",
                                    "Sonoran Basin and Range" = "Sonoran Basin and Range",
                                    "Southern California Mountains" = "Southern California Mountains",
                                    "Southern California/Northern Baja Coast" = "Southern California/Northern Baja Coast"),
                     selected = "Cascades")
)



##### Designing the dashboard Body Layout

body <- dashboardBody(
  tabItems( 
    tabItem(tabName = "dashboard",
            h2(fluidRow(
              #first column, with map and table
              column(width = 8,
                     box(background = "black",
                         width = 12,
                         leafletOutput("map", height = 700, width = 600)),
                     box(width = 12,
                         dataTableOutput('dto', width = 600))),
              #second column with graphs and info boxes
              column(4,
                     fluidRow(width = 4,
                              box(width = 12,
                                  title = "Quick Stats", 
                                  background = "blue",
                                  solidHeader = TRUE,
                                  valueBoxOutput("count", width = 4),
                                  valueBoxOutput("acres", width = 8))),
                     box(width = 14,
                         background = "blue",
                         title = "<b>Fire Causes</b>",
                         plotOutput("causePlot")),
                     plotlyOutput("pie"))))),
   tabItem(tabName = "about",
            h2("About")) 
     #          fluidRow(
      #           row(background = )
       #        ))) #For Camila to incorporate 
    
            ))

#################################################################             
####Camila's code to be incorporated into the "About" tab#########
##################################################################

#navbarPage("Playing with Fire... Data",
           
#           sidebarLayout(
 #            sidebarPanel(
  #             tags$img(src='justin_sullivan_getty_images.jpg', height=150, width=175),
   #            tags$figcaption("A firefighter monitoring the Mendocino Complex fire on Aug. 7, 2018. Justin Sullivan/Getty Images")
    #         ),
     #        mainPanel("A summary of the app, what it does, how to use it and a description of the data (including citations). Plus small background info paragraph on significance of fires in CA")),
      #     tabPanel("Map"),
       #    tabPanel("Graphs",
        #            sidebarLayout(
         #             sidebarPanel(
          #              tags$img(src='thomas_fire.jpg', height=150, width=175),
           #             tags$figcaption("Caption/source")
            #          ),
             #         mainPanel(plotOutput("acresPlot"))))))




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
  
  ########################eco pie##############################
  
  reactive_region <- reactive({
    eco_pie %>% 
      filter(Region == input$checkRegion)
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
      scale_x_continuous(expand = c(0,0), limit = c(1877,2018))+
      scale_y_continuous(expand = c(0,0), limit = c(0, 510))+
      labs(y = "Fire Size (Thousands of Acres)", x = "Year")
  })
  
  
  
  ############################Value Boxes######################
  
  #valuebox 1: count of fires within the selection range
  output$count <- renderValueBox(
    valueBox(
      paste0(nrow(reactive_date())), 
      "Number of Fires", 
      color = "red"
    )
  )
  
  #valuebox 2: summing acres withing selection range
  output$acres <- renderValueBox(
    valueBox(
      paste0(round(sum(reactive_date()$GIS_ACRES)/10000),1), 
      "Total Area Burned (Thousands of Acres)", 
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
  
#reactive pie chart
  output$pie <- renderPlotly({plot_ly(reactive_region(), labels = ~Region, values = ~GIS_ACRES, type = 'pie') %>%
    layout(title = 'Acres Burned',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})
}

# Run the application 
shinyApp(ui = ui, server = server)

