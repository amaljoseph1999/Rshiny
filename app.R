#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin='red',
                    dashboardHeader(title='My dashboard'
                                    ),
                    dashboardSidebar(selectInput("var", "Country:",
                                                 c(ca$Country.Region)) ),
                    dashboardBody(distPlot)

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
  ca <- read.csv(file = 'C:/Users/ArulSamy/Desktop/PP1.csv')
   a=input$var
  
     plot(ca$ConfirmedCases[ca$Country.Region==a],ca$temp[ca$Country.Region==a])
     a2 <- data.frame(Inc =ca$ConfirmedCases[ca$Country.Region==a], Spend = ca$temp[ca$Country.Region==a])
     abline(lm(Spend~Inc,data=a2))
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
