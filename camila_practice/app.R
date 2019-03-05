library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Playing with Fire... Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Region",
                  label = "Eco Region", 
                  choices = c(sort(unique(eco_intersect$Region)),'All'))),
    mainPanel(plotOutput("ecoPlot"))
    
    
    # Show a plot of the generated distribution
    
  ))


# Define server logic required to draw cause plot 
server <- function(input, output, session) {
  
  #wrangle to make reactive data frame for plot with sum of acres burned/year
 # reactive_cause<- reactive({
  #  if(input$cause == 'All') 
   # {top100 %>% 
    #    group_by(YEAR_) %>%
     #   summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
      #  mutate(acres_burn_tot_1000 = acres_burn_tot/1000) 
   # }
    
   # else {top100 %>%
    #    filter(CAUSE == input$cause) %>% 
     #   group_by(YEAR_) %>%
      #  summarize(acres_burn_tot = sum(GIS_ACRES)) %>% 
       # mutate(acres_burn_tot_1000 = acres_burn_tot/1000)
  #  }})
  
  
  #Make plot based on cause
  output$ecoPlot <- renderPlot({
    
    # draw the plot with the specified cause
    ggplot(data = eco_intersect(), aes(x = YEAR_, y = GIS_ACRES))+
      geom_col(fill = "firebrick1", colour = "firebrick4")+
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limit = c(1877,2017))+
      scale_y_continuous(expand = c(0,0), limit = c(0, 510))+
      labs(y = "Fire Size (Thousands of Acres)", x = "Year")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)