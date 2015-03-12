library(rCharts)
library(shiny)
library(shinythemes)
source("shotLog.R")

server <- function(input, output) {
        # type ahead
        
        data <- reactive({
                data <- getShotLog(player_name = input$player_name, season = input$season)
                
        })
        
        output$chart1 <- renderChart2({
                
                n1 <- rPlot(shot_dist ~ shot_clock, data = data(), color = "shot_result", type = "point")
                return(n1)
                
        })
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
        tabPanel('Length menu',        dataTableOutput('ex2')),
        tabPanel('No pagination',      dataTableOutput('ex3')),
        tabPanel('No filtering',       dataTableOutput('ex4')),
        tabPanel('Individual filters', dataTableOutput('ex5')),
        tabPanel('Function callback',  dataTableOutput('ex6'))                  

)

shinyApp(ui = ui, server = server)

