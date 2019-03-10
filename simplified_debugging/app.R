######PRACTICE####


library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(varhandle)
library(DT)
library(shinydashboard)
library(RColorBrewer)
library(plotly)


#library(fontawesome)


#testing github
#####to do list#####
# must do:
# - Pie chart - customize colors CAMILA
# - Quick stats for pie chart are incorrect - CAMILA with help from JENNY & KYLE?
# - Fill out the 'about section' - CAMILA to draft, JENNY & KYLE to edit
# - Bar chart - fix wrangling and make each cause different color JENNY wit KYLE help? 


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


#change projection to be compatible with leaflet
top100 <- st_transform(top100, crs = 4326)



##############################################################################
# UI
##############################################################################
header <- dashboardHeader(title = "Playing With Fire...Data", titleWidth = 250)



####### Sidebar
sidebar <- dashboardSidebar(
  #side bar tabs: 

  
  #eco slider for year selection
  selectInput(inputId = "cause",        
              label = "Cause of Fire", 
              choices = c("All", c(sort(unique(top100$CAUSE))))))
  




##### Designing the dashboard Body Layout

body <- dashboardBody(
 plotOutput("causePlot")
  )

ui <- dashboardPage(header, sidebar, body)

##############################################################################
# Server Side
##############################################################################

server <- function(input, output, session) {
  


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
  
  
  
  
  #####################################reactive bar chart######################################
  #Make plot based on cause
  output$causePlot <- renderPlot({
    
    # draw the plot with the specified cause
    ggplot(data = reactive_cause(), aes(x = YEAR_, y = acres_burn_tot_1000))+
      geom_col(fill = "firebrick1", colour = "firebrick4", width = 5)+
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limit = c(1877,2018))+
      scale_y_continuous(expand = c(0,0), limit = c(0, 510))+
      labs(y = "Fire Size (Thousands of Acres)", x = "Year")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)