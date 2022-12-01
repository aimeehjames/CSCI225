#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
mutate(quality_of_life,total_score=(stability+rights+health+safety+climate+costs+popularity)/7)
ui <- fluidPage(
  
  # Application title
  titlePanel("Countries Total Scores"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("selects", "Total Score:",
                  min = 29,
                  max = 79,
                  value = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    quality_of_life%>%
      mutate(quality_of_life,total_score=(stability+rights+health+safety+climate+costs)/6)%>%
      filter(total_score < input$selects)%>%
      ggplot(mapping=aes(x=reorder(country,total_score),y=total_score,fill=country))+geom_bar(stat="identity",position="dodge")+geom_bar(stat="identity",position="dodge")+scale_x_discrete(guide = guide_axis(n.dodge=3))+ggtitle("Total Scores Filtered for Countries Below the Slider Input")+xlab("Country Names")+ylab("Total Score")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
