library(httr)
library(rvest)
library(jsonlite)

load("./data/player.table.Rda")

playerSearch <- function(player_name = ""){
        id <- player.table[player.table$display_first_last==player_name, person_id]
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

test <- shotStatsURL(playerSearch("Kevin Garnett"), season="2012-13")
# request the URL and parse the JSON
request <- GET(test)

status_code(request)
headers(request)
str(content(request))
head(content(request))

content <- content(request, "text")
john.wall <- fromJSON(content)
lapply(john.wall, dim)

john.wallShots <- data.frame(john.wall$resultSets[[3]])
names(john.wallShots) <- john.wall$resultSets[[2]][[1]]
str(john.wallShots)

