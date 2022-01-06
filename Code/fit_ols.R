#Lee Przybylski
#1/4/2021

#Fit a ols regression model to predict spread for games pulled from scrape.R

library(MASS)
library(Matrix)
library(dplyr)
options(stringsAsFactors = FALSE)

teams <-sort(unique(games$Team))
teams

fit_ols <- function(games){
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
  #Fit the model
  ha_reg1 <- lm(Y ~ X+0)
  return(ha_reg1)
}