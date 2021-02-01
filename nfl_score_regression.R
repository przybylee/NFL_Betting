#Build a linear model to predict a teams score given the defense they are facing
#This can also be used for predicting total scores.
#Lee Przybylski 9/22/2020
library(MASS)
library(Matrix)
library(dplyr)
options(stringsAsFactors = FALSE)

games <- read.csv("nfl_scores_2019_2020.csv", sep = ",")
head(games)
games$X <- NULL
unique(games$Team)
teams <-sort(unique(games$Team))
teams
summary(games)
#We have scores from every game, given as away team then home team.
N = length(games$Score)
games[games$Team == "Washington Redskins","Team"] <- "Washington Football Team"
games[games$Team == "Oakland Raiders","Team"] <- "Las Vegas Raiders"
M = length(unique(games$Team))

scores_df <- data.frame(matrix(data = NA, nrow = N, ncol = 7))
names(scores_df) <- c("score", "team", "opp", "home_away", "week", "year")
head(scores_df)
