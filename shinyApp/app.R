library(rCharts)
library(shinydashboard)
library(shinysky)
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

ui <- dashboardPage(
        dashboardHeader(title = "NBA Player Shot Logs"),
        dashboardSidebar(
                menuItem("Dashboard", tabName = "dashboard", icon=icon("dashboard")),
                menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                
        ),
        dashboardBody(
                # Boxes need to be put in a row (or column)
                
                tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                        column(width=3,
                                               box(width=NULL, status="warning",
                                                   title = "Controls",
                                                   selectizeInput("player_name",
                                                                  label = "Search",
                                                                  choices = selectizePlayers,
                                                                  selected = NULL,
                                                                  multiple = FALSE),
                                                   selectInput("season",
                                                               label = "Pick Season",
                                                               choices = c("2014-15", "2013-14"),
                                                               selected = "2014-15")                                                   
                                               )
                                        )),
                                fluidRow(
                                        column(width=9,
                                               box(width=NULL, solidHeader = TRUE,
                                                   showOutput("chart1", "Polycharts")
                                                   
                                               ))
                                        
                                )
                        )
                )
        )
)
shinyApp(ui = ui, server = server)

