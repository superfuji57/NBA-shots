library(rCharts)
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
source("shotLog.R")

server <- function(input, output) {
        
        # Shot log data for player
        data <- reactive({
                getShotLog(player_name = input$player_name, season = input$season)         
        })
        
        # scatterplot of shot dist vs time left on shot clock
        output$chart1 <- renderChart2({
                n1 <- rPlot(shot_dist ~ shot_clock, data = data(), color = "shot_result", type = "point")
                return(n1) 
        })
        
        # data table
        dataDT <- reactive({data() %>% group_by(closest_defender) %>%
                summarize(fgm = length(shot_result))
        })
        
        output$datatable <- renderDataTable(dataDT(), options=list(pageLength=10))
        
}

ui <- navbarPage(
        title = "Exploring the NBA's Shot Logs",
        tabPanel('First Chart',
                 sidebarLayout(
                         sidebarPanel(
                                 h4("Controls"),
                                 selectizeInput("player_name",
                                                label = "Search",
                                                choices = selectizePlayers,
                                                selected = "Search",
                                                multiple = FALSE),
                                 selectInput("season",
                                             label = "Pick Season",
                                             choices = c("2014-15", "2013-14"),
                                             selected = "2014-15")                                                   
                                 ),
                         mainPanel(
                                 h3("Shots vs Distance"),
                                 showOutput("chart1", "Polycharts")                                 
                                 )
                         )
                 ),
        tabPanel('Length menu',        dataTableOutput('datatable')),
        tabPanel('No pagination',      dataTableOutput('ex3')),
        tabPanel('No filtering',       dataTableOutput('ex4')),
        tabPanel('Individual filters', dataTableOutput('ex5')),
        tabPanel('Function callback',  dataTableOutput('ex6'))                  

)

shinyApp(ui = ui, server = server)

