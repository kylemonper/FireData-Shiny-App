#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#### Data Wrangling and New Data Frame creation space #####


  

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "cause",
                     label = "Cause of Fire", 
                     choices = c(sort(unique(top100$CAUSE)),'All'))),
      mainPanel(plotOutput("causePlot"))
      
      
      # Show a plot of the generated distribution
   
   ))


# Define server logic required to draw cause plot 
server <- function(input, output, session) {
  
  #wrangle to make reactive data frame for plot with sum of acres burned/year
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
  
  #Make variable to fill graph based on cause 
  
  fill_color <- reactive({
    if(input$cause == 'Arson'){"cyan"}
    else if (input$cause == 'Lightning') {"coral1"}
    else if(input$cause == 'Vehicle'){"orange"}
    else if(input$cause == 'Miscellaneous'){"blue"}
    else if(input$cause == 'Campfire'){"yellow"}
    else if(input$cause == 'Unknown/Unidentified'){"purple"}
    else if(input$cause == 'Equipment Use'){"grey"}
    else if(input$cause == 'Debris'){"brown"}
    else if(input$cause == 'Railroad'){"red"}
    else if(input$cause == 'Power Line'){"black"}
    else{}
  })
  #choose a palette/set a palette of 
  
  
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
}

# Run the application 
shinyApp(ui = ui, server = server)

