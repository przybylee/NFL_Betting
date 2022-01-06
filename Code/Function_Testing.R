#Test Functions

source("nfl_scrape.R")
games <- scrape_games(ssn = 2021, wk_stop = 17)
head(games)
tail(games)

#Save scores from 2021 season
write.csv(games, "nfl_scores_2021.csv")

games <- read.csv("nfl_scores_2021.csv")
head(games)

teams <-sort(unique(games$Team))
teams
