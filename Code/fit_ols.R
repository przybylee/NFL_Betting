#Lee Przybylski
#1/4/2021

#Fit a ols regression model to predict spread for games pulled from scrape.R

teams <-sort(unique(games$Team))
teams

fit_ols <- function(games){
  N <- length(games$Score)
  M <- length(unique(games$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  teams <- sort(unique(games$Team))
  
}