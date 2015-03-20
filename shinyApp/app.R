library(rCharts)
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(waffle)
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
                summarize(fg_pct = sum(shot_result=="made") / length(shot_result),
                          avg_dist = mean(shot_dist),
                          avg_dribble = mean(dribbles),
                          fga = length(shot_result),
                          fgm = sum(shot_result == "made"))
        })
        
        output$datatable <- renderDataTable(datatable(dataDT(), options=list(pageLength=10)))
        
        output$waffle1 <- renderPlot({
                waffle_data <- group_by(data(), shot_result) %>%
                        summarize(count = n())
                
                colors <- sample(colors(), length(waffle_data[,1]), replace = FALSE)
                parts <- c(waffle_data$count)
                
                waffle(waffle_data$count/2, colors=c("green", "red"))        
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
        tabPanel('Player Table', dataTableOutput('datatable')),
        tabPanel('Waffle Chart', plotOutput("waffle1"))
)

shinyApp(ui = ui, server = server)

