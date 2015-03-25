library(httr)
library(rvest)
library(jsonlite)
library(lubridate)
library(tidyr)
library(dplyr)
library(rChartsCalmap)
library(rChartsCalendar)

load("./data/player.table.Rda")

selectizePlayers <- player.table[player.table$to_year >= 2013, 4]
selectizePlayers <- as.character(selectizePlayers[!is.na(selectizePlayers)])

playerSearch <- function(player_name = ""){
        id <- player.table[player.table$display_first_last==player_name, 1]
        if (length(id) == 0) return("No player found")
        return(id)
}

# data goes back to 2013-14 season
shotStatsURL <- function(playerID=951, from="", to="", gameSegment="", nGames=0, location="",month=0, outcome="",
                         period=0, season="2014-15", seasonSegment="", seasonType="Regular+Season",
                         teamID=0, vsConference="", vsDivision="") {
        # GET URL
        shots.url <- paste0("http://stats.nba.com/stats/playerdashptshotlog?DateFrom=", from,
                            "&DateTo=", to, 
                            "&GameSegment=", gameSegment,
                            "&LastNGames=",nGames, 
                            "&LeagueID=00",
                            "&Location=", location,
                            "&Month=", month,
                            "&OpponentTeamID=", teamID, 
                            "&Outcome=", outcome="",
                            "&Period=", period,
                            "&PlayerID=", playerID,
                            "&Season=", season,
                            "&SeasonSegment=", seasonSegment,
                            "&SeasonType=", seasonType,
                            "&TeamID=", teamID,
                            "&VsConference=", vsConference,
                            "&VsDivision=", vsDivision)
        return(shots.url)
}

getShotLog <- function(player_name="Ray Allen", season="2014-15") {
        
        player.URL <- shotStatsURL(playerSearch(player_name), season=season)
        # request the URL and parse the JSON
        request <- GET(player.URL)
        # status_code(request)
        # content <- content(request, "text")
        player.json <- fromJSON(content(request, "text"))
        player.data <- data.frame(player.json$resultSets[[3]], stringsAsFactors = FALSE)
        names(player.data) <- tolower(player.json$resultSets[[2]][[1]])
        for (i in c(5, 6, 9, 10, 11, 12, 17, 18, 19)) {
                player.data[,i] <- as.numeric(as.character(player.data[,i]))
        }
        player.data <- separate(player.data, matchup, into = c("date", "matchup"), sep = "-") %>%
                transform(date = mdy(date))
        return(player.data)
        
}

shot_waffle <- function(data, month) {
        data <- filter(data, month(date)==month)
        part_shots <- rep(1, nrow(data))
        names(part_shots) <- data$tip
        colorKey <- rep("red", nrow(data))
        colorKey[data$shot_result=="made"] <- "green"
        waffle(part_shots, colors=colorKey, rows = 5, title = as.character(month(month, label = T, abbr = F)),
               size=1)
}

# heat function cumsum, returns to 0 on a miss

heat <- function(shots){
        heat <- vector()
        for (i in 1:length(shots)){
                if (i == 1) {
                        if (shots[i] == 1) heat[i] <- 1
                        else heat[i] <- 0
                }
                else if (shots[i] == 1) heat[i] <- heat[i-1] + 1
                else heat[i] <- 0
        }
        return(heat)
}

# cal map

heat_data <- function(shotLog) {
        arrange(shotLog, date) %>% 
        transmute(date, shot_result, 
                  tip_def = paste("Defender:", as.character(closest_defender))) %>%
        group_by(month = month(date)) %>%
        mutate(datehour = ymd(paste(year(date[1]), month(date[1]),1, sep = "-")) + hours(row_number()+4),
               made = (shot_result=="made")*1,
               heat = heat(made))
}

result_map <- function(heatData){
        r1 <- calheatmap(x="datehour", y="heat",
                 itemSelector=c('#cellradius-b', '#colLimit-b'),
                 cellSize = 15,
                 cellRadius = 10,
                 tooltip = 'FALSE',
                 data=heatData,
                 domain="month",
                 label = c("position"="left"),
                 domainGutter = 10,
                 subDomain="hour",
                 rowLimit = 10,
                 range=length(unique(heatData$month)),
                 verticalOrientation = "true",
                 displayLegend = "FALSE",
                 start = as.character(heatData$date[1]),
                 legend = seq(.1,1.2,.1),
                 legendColors = c("min"="white", "max"="green", "empty"="lightgrey", "base"="white"))
        return(r1)
}

heat_map <- function(heatData){
        
        r1 <- calheatmap(x="datehour", y="heat",
                         itemSelector=c('#cellradius-b', '#colLimit-b'),
                         cellSize = 15,
                         cellRadius = 10,
                         tooltip = FALSE,
                         data=heatData,
                         domain="month",
                         label = c("position"="left"),
                         domainGutter = 10,
                         subDomain="hour",
                         rowLimit = 10,
                         range=length(unique(heatData$month)),
                         verticalOrientation = "true",
                         displayLegend = "false",
                         start = as.character(heatData$date[1]),
                         legend = seq(.1, 6 , .1),
                         legendColors = c("min"="white", "max"="red", "empty"="lightgrey", "base"="white"))
        return(r1)
}
