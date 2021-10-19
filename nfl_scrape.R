#Author: Chancellor Johnstone, Iowa State University

#code to perform scrape

#uncomment these lines to install packages needed
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("readr")

library(rvest)
library(dplyr)
library(readr)

#scrapes data from pro football reference
nfl_base <- "https://www.pro-football-reference.com/"

#teams involved from 2010 onward; we look for these specific strings
nfl_teams <- c("Green Bay Packers",
               "Chicago Bears",
               "Tennessee Titans",
               "Cleveland Browns",
               "Los Angeles Rams",
               "Carolina Panthers",
               "Washington Redskins",
               "Washington Football Team",
               "Philadelphia Eagles",
               "Buffalo Bills",
               "New York Jets",
               "Atlanta Falcons",
               "Minnesota Vikings",
               "Baltimore Ravens",
               "Miami Dolphins",
               "Kansas City Chiefs",
               "Jacksonville Jaguars",
               "Cincinnati Bengals",
               "Seattle Seahawks",
               "Indianapolis Colts",
               "Los Angeles Chargers",
               "Las Vegas Raiders",
               "San Francisco 49ers",
               "Tampa Bay Buccaneers",
               "New York Giants",
               "Dallas Cowboys",
               "Detroit Lions",
               "Arizona Cardinals",
               "Pittsburgh Steelers",
               "New England Patriots",
               "Houston Texans",
               "New Orleans Saints",
               "Denver Broncos",
               "Oakland Raiders",
               "St. Louis Rams",
               "San Diego Chargers")

#total number of teams
l <- nchar(nfl_teams)

date_list <- c()
#year_step <- 2009
score_step <- 3
scores <- c()

#currently scraping only regular season
#some quick changes can be made to get playoffs

#AWAY TEAM SCORE APPEARS FIRST...
for (year in 2021:2021) {
  for (week in 1:5) {
    #manipulate base string to get a specific week in a specific season
    date <- paste0("years/", year, "/week_", week, ".htm")
    paste0(nfl_base, date) -> url
    
    #execute string scrape
    game <- read_html(url) %>% 
      html_node("#content") %>%
      html_text()
    
    #getting the teams that played during the current week
    hold <- sapply(nfl_teams, regexpr, game) 
    played <- hold[hold > 0]
    ord <- order(played)
    
    #ordering teams that played; eliminates having to check entire list
    played <- played[ord]
    l_play <- l[hold > 0][ord]
    
    #number of teams that played
    n <- length(played)
    
    #getting score information
    day_data <- c()
    if (n > 0) {
      for (t in 1:n) {
        #stepping through string
        score_loc <- played[t] + l_play[t] + score_step
        
        #collecting scores... plus garbage
        score_text <- substr(game, score_loc, score_loc + 3)
        
        #read score
        score <- readr::parse_number(score_text) 
        
        #add to weekly data frame
        new <- c(names(played)[t], score, year, week)
        day_data <- rbind(day_data, new)
      }
    }
    
    #add to entire list of scores
    scores <- rbind(scores, day_data)
  }
}

#write to csv file
scores <- as.data.frame(scores)
names(scores) <- c("Team", "Score", "Year", "Week")
head(scores)
tail(scores)
summary(scores)
scores <- scores[!is.na(scores$Score),]
write.csv(scores, "nfl_scores_2019_2020.csv")

