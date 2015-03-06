library(httr)
library(rvest)
library(jsonlite)

load("./data/player.table.Rda")

playerSearch <- function(player_name = ""){
        id <- player.table[player.table$display_first_last==player_name, 1]
        if (length(id) == 0) return("No player found")
        return(id)
}

# data goes back to 2013-14 season
shotStatsURL <- function(playerID=951, from="", to="", gameSegment="", nGames=0, location="",month=0, outcome="",
                         period=0, season="2014-15", seasonSegment="", seasonType="Regular+Season",
                         teamID=0, vsConference="", vsDivision="") {
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
        player.data <- data.frame(do.call(rbind,player.json$resultSets[[1]]$rowSet))
        names(player.data) <- tolower(player.json$resultSets[[1]][2]$headers)
        for (i in c(5, 6, 9, 10, 11, 12, 17, 18, 19)) {
                player.data[,i] <- as.numeric(as.character(player.data[,i]))
        }
        return(player.data)
        
}