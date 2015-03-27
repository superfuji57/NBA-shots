library(rCharts)
library(shiny)
library(shinythemes)
#library(DT)
library(dplyr)
library(htmlwidgets)
library(tidyr)
library(lubridate)
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
        
        ## waffle charts code
        heatData <- reactive({heat_data(data())})
        
        output$heat <- renderCalheatmap({
                        data <- heatData()
                        chart <- result_map(data)
                        return(chart)
                })
         
        #output$test <- renderText(heatData()$tip_def[1])
        #output$waffle1 <- renderPlot({
                #data %>% group_by(month(date)) %>%
                #        summarize(count=n())
         #       x
          #      })
        
}

ui <- fluidPage(
        title = "Exploring the NBA's Shot Logs",
        fluidRow(
                column(width=3,
                       h4("Controls")),
                column(width= 5,
                       selectizeInput("player_name",
                                label = "Search",
                                choices = selectizePlayers,
                                selected = "Search",
                                multiple = FALSE), 
                       selectInput("season",
                             label = "Pick Season",
                             choices = c("2014-15", "2013-14"),
                             selected = "2014-15")                                                   
                 )),
        fluidRow(
                column(width=6,
                       calheatmapOutput("heat",width = 20))
                )
        )

shinyApp(ui = ui, server = server)

