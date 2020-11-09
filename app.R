#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")


#------------------------------------DATA ACCESS AND CLEANING-----------------------------------------------------------------------------


data_confirmed<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",sep=',', head=TRUE,check.names =FALSE )
data_deceased<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",sep=',', head=TRUE,check.names =FALSE)
data_recovered<-read.csv("C:/Users/ArulSamy/Downloads/COVID-19-master/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",sep=',', head=TRUE,check.names =FALSE)
##View(data_con)

current_date <- as.Date(names(data_confirmed)[ncol(data_confirmed)], format = "%m/%d/%y")
changed_date <- file_info("data/covid19_data.zip")$change_time

# Get evolution data by country
data_confirmed_sub <- data_confirmed %>%
    pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
    group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
    summarise("confirmed" = sum(value, na.rm = T))

data_recovered_sub <- data_recovered %>%
    pivot_longer(names_to = "date", cols = 5:ncol(data_recovered)) %>%
    group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
    summarise("recovered" = sum(value, na.rm = T))

data_deceased_sub <- data_deceased %>%
    pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
    group_by(`Province/State`, `Country/Region`, date, Lat, Long) %>%
    summarise("deceased" = sum(value, na.rm = T))

data_evolution <- data_confirmed_sub %>%
    full_join(data_deceased_sub) %>%
    ungroup() %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    arrange(date) %>%
    group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
    mutate(
        recovered = lag(confirmed, 14, default = 0) - deceased,
        recovered = ifelse(recovered > 0, recovered, 0),
        active = confirmed - recovered - deceased
    ) %>%
    pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
    ungroup()

# Calculating new cases
data_evolution <- data_evolution %>%
    group_by(`Province/State`, `Country/Region`) %>%
    mutate(value_new = value - lag(value, 4, default = 0)) %>%
    ungroup()

#---------------------------------------------------------DASH BOARD-----------------------------------------------------

# Define UI for application that draws a histogram


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
    ),
    width = 180)

#----------------------------------------------------DASHBOARD BODY---------------------------------------------------------
body <- dashboardBody(tags$head(tags$style(HTML('.content-wrapper,.right-side{background-color:#c4c4c4}'))),
    tabItems(
        tabItem(tabName = "dashboard", fluidRow(
          valueBoxOutput("rate",width = 3),
          valueBoxOutput("rate1",width = 3),
          valueBoxOutput("rate2",width = 3),
          valueBoxOutput("rate3",width = 3)
        ),
                dataTableOutput("fullTable")
        
        ),
        
        tabItem(tabName = "widgets",
                fluidRow(
                  column(width = 9,
                         box(width = NULL, solidHeader = TRUE,
                             leafletOutput("bar1", height = 500)
                         ),
                         box(width = NULL
                           
                         )
                  ),
                  column(width = 3,
                         box(width = NULL, status = "warning",
                             uiOutput("routeSelect"),
                             checkboxGroupInput("directions", "Show",
                                                choices = c(
                                                  Northbound = 4,
                                                  Southbound = 1,
                                                  Eastbound = 2,
                                                  Westbound = 3
                                                ),
                                                selected = c(1, 2, 3, 4)
                             ),
                             p(
                               class = "text-muted",
                               paste("Note: a route number can have several different trips, each",
                                     "with a different path. Only the most commonly-used path will",
                                     "be displayed on the map."
                               )
                             ),
                             actionButton("zoomButton", "Zoom to fit buses")
                         ),
                         box(width = NULL, status = "warning",
                             selectInput("interval", "Refresh interval",
                                         choices = c(
                                           "30 seconds" = 30,
                                           "1 minute" = 60,
                                           "2 minutes" = 120,
                                           "5 minutes" = 300,
                                           "10 minutes" = 600
                                         ),
                                         selected = "60"
                             ),
                             uiOutput("timeSinceLastUpdate"),
                             actionButton("refresh", "Refresh now"),
                             p(class = "text-muted",
                               br(),
                               "Source data updates every 30 seconds."
                             )
                         )
                  )
                )
        )
    )
)




header <- dashboardHeader(
    title = "COVID-19",
    
    
    
    
    # Dropdown menu for notifications
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 notificationItem(icon = icon("users"), status = "info",
                                  "5 new members joined today"
                 ),
                 notificationItem(icon = icon("warning"), status = "danger",
                                  "Resource usage near limit."
                 ),
                 notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                  status = "success", "25 sales made"
                 ),
                 notificationItem(icon = icon("user", lib = "glyphicon"),
                                  status = "danger", "You changed your username"
                 )
    ),
    
    # Dropdown menu for tasks, with progress bar
    dropdownMenu(type = "tasks", badgeStatus = "danger",
                 taskItem(value = 20, color = "aqua",
                          "Refactor code"
                 ),
                 taskItem(value = 40, color = "green",
                          "Design new layout"
                 ),
                 taskItem(value = 60, color = "yellow",
                          "Another task"
                 ),
                 taskItem(value = 80, color = "red",
                          "Write documentation"
                 )
    )
)

# Put them together into a dashboardPage


ui<-dashboardPage(skin = 'purple',
    header,
    sidebar,
    body
)




# Define server logic required to draw a histogram

#----------------------------------------SERVER------------------------------------------------------


server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })


#------------------------------------TABLE DATA-------------------------------------------------------------------------
    

    getFullTableData <- function(groupBy) {
        padding_left <- max(str_length(data_evolution$value_new), na.rm = TRUE)
        data         <- data_evolution %>%
            filter(date == current_date) %>%
            pivot_wider(names_from = var, values_from = c(value, value_new)) %>%
            select(-date, -Lat, -Long) %>%
            add_row(
                "Province/State"      = "World",
                "Country/Region"      = "World",
                
                "value_confirmed"     = sum(.$value_confirmed, na.rm = T),
                "value_new_confirmed" = sum(.$value_new_confirmed, na.rm = T),
                "value_recovered"     = sum(.$value_recovered, na.rm = T),
                "value_new_recovered" = sum(.$value_new_recovered, na.rm = T),
                "value_deceased"      = sum(.$value_deceased, na.rm = T),
                "value_new_deceased"  = sum(.$value_new_deceased, na.rm = T),
                "value_active"        = sum(.$value_active, na.rm = T),
                "value_new_active"    = sum(.$value_new_active, na.rm = T)
            ) %>%
            group_by(!!sym(groupBy)) %>%
            summarise(
                confirmed_total     = sum(value_confirmed, na.rm = T),
                confirmed_new       = sum(value_new_confirmed, na.rm = T),
                confirmed_totalNorm = round(sum(value_confirmed, na.rm = T)),
                recovered_total     = sum(value_recovered, na.rm = T),
                recovered_new       = sum(value_new_recovered, na.rm = T),
                deceased_total      = sum(value_deceased, na.rm = T),
                deceased_new        = sum(value_new_deceased, na.rm = T),
                active_total        = sum(value_active, na.rm = T),
                active_new          = sum(value_new_active, na.rm = T),
                active_totalNorm    = round(sum(value_active, na.rm = T))
            ) %>%
            mutate(
                "confirmed_newPer" = confirmed_new / (confirmed_total - confirmed_new) * 100,
                "recovered_newPer" = recovered_new / (recovered_total - recovered_new) * 100,
                "deceased_newPer"  = deceased_new / (deceased_total - deceased_new) * 100,
                "active_newPer"    = active_new / (active_total - active_new) * 100
            ) %>%
            mutate_at(vars(contains('_newPer')), list(~na_if(., Inf))) %>%
            mutate_at(vars(contains('_newPer')), list(~na_if(., 0))) %>%
            mutate(
                confirmed_new = str_c(str_pad(confirmed_new, width = padding_left, side = "left", pad = "0"), "|",
                                      confirmed_new, if_else(!is.na(confirmed_newPer), sprintf(" (%+.2f %%)", confirmed_newPer), "")),
                recovered_new = str_c(str_pad(recovered_new, width = padding_left, side = "left", pad = "0"), "|",
                                      recovered_new, if_else(!is.na(recovered_newPer), sprintf(" (%+.2f %%)", recovered_newPer), "")),
                deceased_new  = str_c(str_pad(deceased_new, width = padding_left, side = "left", pad = "0"), "|",
                                      deceased_new, if_else(!is.na(deceased_newPer), sprintf(" (%+.2f %%)", deceased_newPer), "")),
                active_new    = str_c(str_pad(active_new, width = padding_left, side = "left", pad = "0"), "|",
                                      active_new, if_else(!is.na(active_newPer), sprintf(" (%+.2f %%)", active_newPer), ""))
            ) %>%
            as.data.frame()
    }
    output$fullTable <- renderDataTable({
        data       <- getFullTableData("Country/Region")
        columNames <- c(
            "Country",
            "Total Confirmed",
            "New Confirmed",
            "Total Confirmed <br>(per 100k)",
            "Total Estimated Recoveries",
            "New Estimated Recoveries",
            "Total Deceased",
            "New Deceased",
            "Total Active",
            "New Active",
            "Total Active <br>(per 100k)")
        datatable(
            data,
            rownames  = FALSE,
            colnames  = columNames,
            escape    = FALSE,
            selection = "none",
            options   = list(
                pageLength     = -1,
                order          = list(8, "desc"),
                scrollX        = TRUE,
                scrollY        = "calc(100vh - 250px)",
                scrollCollapse = TRUE,
                dom            = "ft",
                server         = FALSE,
                columnDefs     = list(
                    list(
                        targets = c(2, 5, 7, 9),
                        render  = JS(
                            "function(data, type, row, meta) {
              if (data != null) {
                split = data.split('|')
                if (type == 'display') {
                  return split[1];
                } else {
                  return split[0];
                }
              }
            }"
                        )
                    ),
                    list(className = 'dt-right', targets = 1:ncol(data) - 1),
                    list(width = '100px', targets = 0),
                    list(visible = FALSE, targets = 11:14)
                )
            )
        ) %>%
            formatStyle(
                columns    = "Country/Region",
                fontWeight = "bold"
            ) %>%
            formatStyle(
                columns         = "confirmed_new",
                valueColumns    = "confirmed_newPer",
                backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
                color           = styleInterval(75, c("#000000", "#FFFFFF"))
            ) %>%
            formatStyle(
                columns         = "deceased_new",
                valueColumns    = "deceased_newPer",
                backgroundColor = styleInterval(c(10, 20, 33, 50, 75), c("NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
                color           = styleInterval(75, c("#000000", "#FFFFFF"))
            ) %>%
            formatStyle(
                columns         = "active_new",
                valueColumns    = "active_newPer",
                backgroundColor = styleInterval(c(-33, -20, -10, 10, 20, 33, 50, 75), c("#66B066", "#99CA99", "#CCE4CC", "NULL", "#FFE5E5", "#FFB2B2", "#FF7F7F", "#FF4C4C", "#983232")),
                color           = styleInterval(75, c("#000000", "#FFFFFF"))
            ) %>%
            formatStyle(
                columns         = "recovered_new",
                valueColumns    = "recovered_newPer",
                backgroundColor = styleInterval(c(10, 20, 33), c("NULL", "#CCE4CC", "#99CA99", "#66B066"))
            )
    })

    
    output$bar1 <- renderLeaflet({
        
        
     x <- raw.data.confirmed
    x$confirmed <- x[, ncol(x)]
    x %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
      mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
    leaflet(width=1200, height=800) %>% addTiles()
    # circle marker (units in pixels)
    m %>% addCircleMarkers(x$Long, x$Lat,
                            # radius=2+log2(x$confirmed),
                            radius=0.03*sqrt(x$confirmed),
                            stroke=F,
                            color='red', fillOpacity=0.3,
                            popup=x$txt)
    # world
        
            })
    
    
    output$rate <- renderValueBox({
     
      cur_dat=Sys.Date()
      valueBox(
        value = data_evolution %>% filter(data_evolution$date=="2020-3-22",data_evolution$var=="confirmed") %>% select(value) %>% sum(na.rm=TRUE),
        subtitle = "Confirmed Cases:",
        icon = icon("area-chart"),width = 2,
        color = "yellow"
      )
    })
    output$rate1<- renderValueBox({
      
      
      valueBox(
        value = data_evolution %>% filter(data_evolution$date=="2020-3-22",data_evolution$var=="recovered") %>% select(value) %>% sum(na.rm=TRUE),
        subtitle = "Recovered Cases:",
        icon = icon("angle-double-down"),width = 2,
        color = "blue"
      )
    })
    output$rate2 <- renderValueBox({
      
      
      valueBox(
        value = data_evolution %>% filter(data_evolution$date=="2020-3-22",data_evolution$var=="deceased") %>% select(value) %>% sum(na.rm=TRUE),
        subtitle = "Deceased:",
        icon = icon("angle-double-up"),width = 2,
        color = "red"
      )
    })
    output$rate3 <- renderValueBox({
      
      
      valueBox(
        value = data_evolution %>% filter(data_evolution$date=="2020-3-22",data_evolution$var=="active") %>% select(value) %>% sum(na.rm=TRUE),
        subtitle = "Active Cases:",
        icon = icon("bed"),width = 2,
        color = "green"
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
