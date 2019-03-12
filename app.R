library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(shinydashboard)
library(RColorBrewer)
library(plotly)
library(ggrepel)


#library(fontawesome)


#testing github
#####to do list#####
# must do:
# - Pie chart - customize colors CAMILA
# - Bar chart - fix wrangling and make each cause different color JENNY wit KYLE help? 

#cool to do
# - make data table reactive - KYLE

##############################################################################
# Wrangling
##############################################################################


#select top 100 fires 
top100 <- fire %>% 
  dplyr::select(YEAR_, FIRE_NAME,GIS_ACRES, CAUSE) %>% 
  arrange(-GIS_ACRES) %>% 
  head(100) %>% 
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

top100$YEAR_ <- unfactor(top100$YEAR_)



#simplify polygons
top100 <- top100 %>% 
  st_simplify(dTolerance = 100)

#add centriods to df to be used in marker creation
centroid <- st_centroid(top100)
centroid_less <- centroid %>% 
  mutate(lat = unlist(map(centroid$geometry,1)),
         long = unlist(map(centroid$geometry,2))) %>% 
  dplyr::select(GIS_ACRES, lat, long) %>% 
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

eco_pie <- data.frame(eco_pie_df)
 
#wrangling for ecoregion map
eco_center <- st_centroid(eco)

eco_map <- eco_center %>% 
  mutate(lat = unlist(map(eco_center$geometry, 1)),
         long = unlist(map(eco_center$geometry, 2))) %>% 
  mutate(region_abbr = 
           case_when(
             Region  == "Cascades" ~ "CASC",
             Region  == "Coast Range" ~ "CR",
             Region  == "Central Basin and Range" ~ "CBR",
             Region  == "Central California Foothills and Coastal Mountains" ~ "CCFCM",
             Region  == "Central California Valley" ~ "CCV",
             Region  == "Eastern Cascades Slopes and Foothills" ~ "ECSF",
             Region  == "Klamath Mountains/California High North Coast Range" ~ "KM/NCR",
             Region  == "Mojave Basin and Range" ~ "MBR",
             Region  == "Northern Basin and Range" ~ "NBR",
             Region  == "Sierra Nevada" ~ "SN",
             Region  == "Sonoran Basin and Range" ~ "SBR",
             Region  == "Southern California Mountains" ~ "SCM",
             Region  == "Southern California/Northern Baja Coast" ~ "SCNBC"))

color_count <- 13
my_colors <- colorRampPalette(brewer.pal(10, "Dark2"))(color_count) # customize color palette if you need more.

##############################################################################
# UI
##############################################################################
header <- dashboardHeader(title = "Playing With Fire...Data", titleWidth = 250)



####### Sidebar
sidebar <- dashboardSidebar(
  
  #side bar tabs: 
  sidebarMenu(width = 2,
    style = "position: fixed;width:16%;",
    menuItem("About", tabName = "about", icon = icon("fab fa-info-circle",lib='font-awesome')),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
    id = "tabs",
 
  
  #map slider for year selection
  sliderInput("date_range",               
              label = "Select Date", 
              min = min(top100$YEAR_), 
              max = max(top100$YEAR_),
              value = range(top100$YEAR_),
              step = 1,
              sep = "",
              width = 400),
  
  #eco slider for number of fires selection
  tags$div(title="Select the top fires within the above date range",
   sliderInput("fire_count",               
              label = "Select Number of Fires (Largest - Smallest)", 
              min = 1, 
              max = 1000,
              value = 1000,
              step = 1,
              sep = "",
              width = 400)),
  
  #select Causes
  selectInput(inputId = "cause",        
              label = "Cause of Fire", 
              choices = c("All", c(sort(unique(top100$CAUSE))))),
  
 

#Get Source Code tab
menuItem("Get Code", icon = icon("fab fa-github",lib='font-awesome'), 
         href = "https://github.com/kylemonper/FireData-Shiny-App")))


##### Designing the dashboard Body Layout

body <- dashboardBody(
  tabItems( 
    tabItem(tabName = "dashboard",
            h2(fluidRow(
              #first column, with map and table
              column(width = 8,
                     box(background = "black",
                         width = 12,
                         leafletOutput("map", height = 400, width = 625)),
                     box(width = 12,
                         #title = strong("Quick Stats for Map"), 
                         background = "black",
                         solidHeader = TRUE,
                         valueBoxOutput("count", width = 4),
                         valueBoxOutput("acres", width = 8))
                     ),
                     
              #second column with graphs and info boxes
              column(4,
                     fluidRow(width = 4,
                     box(width = 14,
                         background = "black",
                         title = strong("Acres Burned by Cause", align = "center"),
                         plotOutput("causePlot")))
                  ))),
            
            fluidRow(
              box(width = 6,
                  height = 600,
                  background = "black",
                  title = strong("Proportion of Fire Occurances by Ecoregion"),
                  plotlyOutput("pie", height = 550)
                  ),
              box(width = 6,
                  background = "black",
                  height = 600,
                  title = strong("Map of California Ecoregions", align = "center"),
                  plotOutput("ecoMap", height = 550)
                  )
            )
            ),
   #About tab
    tabItem(tabName = "about", 
              fluidRow(
                column(width = 3,
                        box(width = 12,
                            tags$img(src='justin_sullivan_getty_images.jpg', height=150, width=175),
                            h5("A firefighter monitoring the Mendocino Complex fire on Aug. 7, 2018. Source: Justin Sullivan/Getty Images"),
                            tags$img(src='thomas_fire.jpg', height=150, width=175),
                            h5("The Santa Barbara Thomas Fire of 2017 - 2018. Source: CNN"))),
            column(8,
                   fluidRow(width = 9,
                            box(width = 12,
                                h3(strong("Welcome to Playing with Fire...Data", align = "center")),
                                tags$p("Visually explore the top 1000 fires in the last 140 years of California’s fire history. See where fires have occurred in the state over different time frames, which Ecoregions have had the biggest fires, and how total acres burned varies among different causal mechanisms."),
                                tags$p("Use the widgets on the left hand side of the dashboard to:
"),
                                tags$ul(
                                  tags$li("select a date range for the map and explore fire perimeters across the state;"),
                                  tags$li("select a cause of fire and have a look at acres burned over time;"), 
                                  tags$li("select the number of fires to include in the pie chart and view the proportion of fires in each Ecoregion. In this widget, 1 represents the largest fire in the state’s history and 1000 represents the smallest (of fires shown in this app). Note that a single fire can occur in multiple Ecoregions.")),
                                  tags$p("The Quick Stats box summarizes the fires displayed on the map based on date range selection."),
                                  tags$p(strong("Data Sources"), align = "left"),
                                  tags$ul(
                                    tags$li(a(href = "http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download", "Fire perimeter data was sourced from the Cal Fire’s Fire and Resource Assessment Program"),
                                    tags$li(a(href = "https://www.epa.gov/eco-research/ecoregion-download-files-state-region-9", "California Ecoregion data was sourced from the United States Environmental Protection Agency"))
                                  
                                  )))
                                
                                )
                            )
            ))))

ui <- dashboardPage(header, sidebar, body)

##############################################################################
# Server Side
##############################################################################

server <- function(input, output, session) {
  
  ##############Reactive Variables###################
  
  #create new reactive df based on slider date inpute in the ui
  reactive_date <- reactive({
    top100 %>%
      filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
      arrange(-GIS_ACRES) %>% 
      head(input$fire_count)
  })
  
  #working on universal reactivity
  # reactive_date <- reactive({
  #   if (is.na(input$cause)){
  #     top100
  #   } 
  #   else (
  #     top100 %>% 
  #       filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2] & CAUSE == input$cause)
  #   )
  # })
  
  ########################eco pie##############################
  
  #Check boxes for eco region
 # reactive_region <- reactive({
  #  eco_pie %>% 
   #   filter(Region == input$checkRegion)
 # })
  
  #Slider for eco region
 reactive_firecount <- reactive({
    eco_pie %>%
     filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
     arrange(-GIS_ACRES) %>% 
      head(input$fire_count)
  })

  ########################cause plot###########################

  reactive_cause<- reactive({
    if(input$cause == 'All') 
    {reactive_date() %>% 
        filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
        group_by(YEAR_) %>%
        summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
        mutate(acres_burn_tot_1000 = acres_burn_tot/1000) 
    }
    
    else {reactive_date() %>%
        filter(CAUSE == input$cause) %>% 
        filter(YEAR_ >= input$date_range[1] & YEAR_ <= input$date_range[2]) %>% 
        group_by(YEAR_) %>%
        summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
        mutate(acres_burn_tot_1000 = acres_burn_tot/1000)
    }})
  
  

  
  ############################Value Boxes######################
 
### Value boxes for map  
   
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
  
###############################reactive pie chart##################################
  
  ecocolors <- c('rgb(70,130,180)', 'rgb(46,139,87)', 'rgb(128,128,0)', 'rgb(0,128,128)', 'rgb(222,184,135)','rgb(188,143,143)', 'rgb(184,134,11)', 'rgb(160,82,45)', 'rgb(105,105,105)', 'rgb(47,79,79)','rgb(112,128,144)', 'rgb(112,128,144)')
  
  output$pie <- renderPlotly({
    
    
    plot_ly(reactive_firecount(),
            labels = ~Region, 
            type = 'pie',
           # marker = list(color = c('rgb(70,130,180)', 'rgb(46,139,87)', 'rgb(128,128,0)', 'rgb(0,128,128)', 'rgb(222,184,135)','rgb(188,143,143)', 'rgb(184,134,11)', 'rgb(160,82,45)', 'rgb(105,105,105)', 'rgb(47,79,79)','rgb(112,128,144)','rgb(112,128,144)'))) %>% 
           marker = list(colors = ecocolors)) %>%
      layout(legend = list(orientation = 'h')) %>% 
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
           #legend = list(x = 1, y = 1)
           )
      
    
})

#####################################reactive bar chart######################################
  
  
  #Make plot based on cause
  output$causePlot <- renderPlot({
    
    #change x-axis reactively
   low <- input$date_range[1] - 1
   high <- input$date_range[2] +1
    
    # draw the plot with the specified cause
    ggplot(data = reactive_cause(), aes(x = YEAR_, y = acres_burn_tot_1000)) +
      geom_col(position = position_stack(), fill = "firebrick3") +
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limit = c(low,high))+
      scale_y_continuous(expand = c(0,0), limit = c(0, 1000))+
      labs(y = "Acres Burned (Thousands of Acres)", x = "Year")
  })

  
  ########ECO-REGION MAP####################
  
  
  output$ecoMap <- renderPlot({
    ggplot(eco) +
    geom_sf(aes(fill = Region),
            color = "NA",
            size = 0.1,
            show.legend = FALSE) +
      scale_fill_manual(values = my_colors) +
      theme_classic() +
      #theme(legend.position = "bottom", legend.box = "vertical") +
      coord_sf(datum = NA) +
    geom_label_repel(data = eco_map, aes(x = lat, y = long, label = region_abbr), label.padding = 0.25,
                     box.padding = 0.6)+
     labs(x = "", y = "")
   #  geom_label_repel(data = eco, )
      
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

