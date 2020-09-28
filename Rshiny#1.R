#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("cerulean"),

    # Application title
    titlePanel("concatination"),
navbarPage(
    # Sidebar with a slider input for number of bins 
    tabPanel("Navigater 1",
        sidebarPanel
        (
            tags$h3("input"),
            textInput("txt1","First name"),
            textInput("txt2","Middle name"),
            textInput("txt3","Last name")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
           h4("Output 1"),
           verbatimTextOutput("txtout"),
                    ),
    tabPanel("Navigater 1","none"),
    tabPanel("Navigater 2","none"),
        )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) 
{
output$txtout<-renderText(
    {
        paste(input$txt1,input$txt2,input$txt3,sep=":")
    }
)
    
 }

# Run the application 
shinyApp(ui = ui, server = server)
