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


source("UI/ui_about.R",local = TRUE)

ui<-fluidPage(
    
    title='hshdh',
    tags$head("hai"),
    navbarPage(
        title = "Covid_19",style="padding-left:10px"),
    collapsible=TRUE,
    fluid=TRUE,
    tabPanel("About",page_about,value = "page-about")
    
 )

server <- function(input, output) {
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)




