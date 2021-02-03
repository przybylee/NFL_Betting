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
names(scores_df) <- c("score", "team", "opp", "home_away", "game_id", "week", "year")
head(scores_df)
I_scores <- 1:N
home_away <- (I_scores+1) %% 2 
id <- 1
for (i in I_scores){
  scores_df$score[i] <- games$Score[i]
  scores_df$team[i] <- games$Team[i]
  scores_df$week[i] <- games$Week[i]
  scores_df$year[i] <- games$Year[i]
  scores_df$game_id[i] <- id
  if (home_away[i] == 0){
    scores_df$opp[i] <- games$Team[i+1]
  }else{
    scores_df$opp[i] <- games$Team[i-1]
    id <- id +1
  }
}
scores_df$home_away <- as.factor(home_away)
scores_df$game_id <- as.factor(scores_df$game_id)
head(scores_df)

#Fit the a basic linear model to predict a score based on a team, 
#opponent, and home/away
lm1 <- lm(score ~ team + opp + home_away, data = scores_df)
summary(lm1)
anova(lm1)

tm1 <- "Tampa Bay Buccaneers"
tm2 <- "Kansas City Chiefs"
game <-data.frame(team = c(tm1,tm2), opp = c(tm2,tm1), home_away = factor(c(1,0)))
predict(lm1, game)
