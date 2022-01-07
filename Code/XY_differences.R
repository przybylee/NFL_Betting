#Lee Przybylski
#1/4/2021

#Fit a ols regression model to predict spread for games pulled from scrape.R

library(MASS)
library(Matrix)
library(dplyr)
options(stringsAsFactors = FALSE)

teams <-sort(unique(games$Team))
teams

XY_differences <- function(games){
  N <- length(games$Score)
  M <- length(unique(games$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  teams <- sort(unique(games$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  colnames(X) <- c("home", teams)
  Y <- rep(0, N/2)
  #Fill in cols of X with 1's where appropriate
  row = 1
  for(n in seq(2,N, by = 2)){
    X[row,"home"] <- 1
    X[row, games$Team[n-1]] <- -1
    X[row, games$Team[n]] <- 1
    Y[row] <- games$Score[n] - games$Score[n-1]
    row <- row +1
  }
  start <- paste(games$Year[1], ", week ", games$Week[1])
  end <- paste(games$Year[N], ", week ", games$Week[N])
  result <- list(X,Y, teams, games, start, end)
  names(result) <- c("X", "Y_diff", "teams", "games", "start", "end")
  return(result)
}

games
test <- XY_differences(games)
test
test$end
