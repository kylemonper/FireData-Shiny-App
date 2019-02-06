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
                     choices = sort(unique(top100$CAUSE)))),
      mainPanel(
        plotOutput("causePlot")
      )
      )
      
      # Show a plot of the generated distribution
   
   )


# Define server logic required to draw cause plot 
server <- function(input, output) {
  #wrangle to make reactive data frame for plot with sum of acres burned/year
  reactive_cause<- reactive({
    top100 %>%
      filter(CAUSE == input$cause) %>% 
      group_by(YEAR_) %>% 
      summarize(acres_burn_tot = sum(Shape_Area))#shape_area place holder for acres_burned, need to update when possible
  })
    #Make plot based on cause
   output$causePlot <- renderPlot({
      
      # draw the plot with the specified cause
      ggplot(reactive_cause, aes(x = Year_, y = acres_burn_tot))+
       geom_line()+
       theme_classic()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

