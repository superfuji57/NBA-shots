library(rCharts)
library(shiny)
library(shinydashboard)
source("shotLog.R")

server <- function(input, output) {
        data <- reactive({
                data <- getShotLog(player_name = input$player, season = input$season)
                
        })
        
        output$chart1 <- renderChart2({
                
                n1 <- rPlot(shot_dist ~ shot_clock, data = data(), color = "shot_result", type = "point")
                return(n1)
                
        })
}

ui <- fluidPage(
        sidebarLayout(
                sidebarPanel(
                        selectInput("player",
                                    label = "Choose Player",
                                    choices = c("John Wall", "Deron Williams"),
                                    selected = "John Wall"),
                        selectInput("season",
                                    label = "Pick Season",
                                    choices = c("2014-15", "2013-14"),
                                    selected = "2014-15")
                ),
                mainPanel(showOutput("chart1", "Polycharts"))
        )
)

shinyApp(ui = ui, server = server)

