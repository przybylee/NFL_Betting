#Build a linear model to predict game outcomes, specifically the margin of victory for the home team
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
X = matrix(data = 0, nrow = N/2, ncol = M+1)
teams <-sort(unique(games$Team))
teams
colnames(X) <- c("home", teams)
Y <- rep(0, N/2)

row = 1
for(n in seq(2,N, by = 2)){
  X[row,"home"] <- 1
  X[row, games$Team[n-1]] <- -1
  X[row, games$Team[n]] <- 1
  Y[row] <- games$Score[n] - games$Score[n-1]
  row <- row +1
}

ha_reg1 <- lm(Y ~ X+0)
summary(ha_reg1)
#Find a confidence interval
#Requires model matrix X, observations y, estimated line d, and confidence level a
conf_int=function(X,y,c,d=0,a=0.05){
  reg <- lm(y ~ X+0)
  c = as.matrix(c)
  X.X <- t(X)%*%X
  df = nrow(X) - anova(reg)[1,1]
  #df= nrow(X) - rankMatrix(X)[1]
  cb.d=t(c)%*%ginv(X.X)%*%t(X)%*%y-d
  sigsq <- anova(reg)[2,3]
  var.cb <- sigsq*t(c)%*%ginv(X.X)%*%c
  tquant <- qt(1-a/2,df)
  std.dev = sqrt(var.cb)
  lower = cb.d - tquant%*%std.dev
  upper = cb.d + tquant%*%std.dev
  data.frame(est = cb.d, std.dev = std.dev, lower = lower, upper = upper)
}

#Name home team and away team to estimate difference in team effects plus home advantage
teams
AwayTm = "Kansas City Chiefs"
HomeTm = "Tampa Bay Buccaneers"
#Enter the moneyline
line_away <- -163
line_home <- 143
spread <- -3
#colnames(X)
c <- c(1,rep(0, length(teams)))
c[colnames(X) == HomeTm] <- 1
c[colnames(X) == AwayTm] <- -1
#Put contrast vector c into confint to estimate HomeTm score - AwayTm score
conf_int(X, y = Y, c = c, d = 0, a = 0.05)
#Using the estimated difference in team effects, compute a win probability
mu <- conf_int(X, y = Y, c = c, d = 0, a = 0.05)[1,1]
sigma <- sqrt(sigsq <- anova(lm(Y ~ X+0))[2,3])
AwayProb <- pnorm(0,mean = mu, sd = sigma)
HomeProb <- pnorm(0,mean = mu, sd = sigma, lower.tail = FALSE)
print(paste("The estimated win probability for the ", AwayTm, "at", HomeTm, "is", AwayProb, sep = " " ))
#Compute the expected winnings for a $100 bet
#Payouts for away then home
line <- c(line_away, line_home)
payouts <- c(100,100)
for(j in 1:2){
  l <- line[j]
  if (l < 0){
      payouts[j] <- 100^2/-l 
  }else{
    payouts[j] <- l
  }
}
exp_away <- AwayProb*payouts[1] - 100*HomeProb
exp_home <- HomeProb*payouts[2] - 100*AwayProb
print(paste("Exp winnings for", AwayTm, "is", exp_away, sep = " "))
print(paste("Exp winnings for", HomeTm, "is", exp_home, sep = " "))

#Here is some code to plot the curve
x <- seq(-30,30)
y <- dnorm(x, mean = mu, sd = sigma)
title = paste("Spread =", spread, "Prediction =", round(mu,2), "std. eror =", round(sigma,2), sep = " ")
plot(x,y, type = "l", xlab = paste(HomeTm, "margin of victory", sep = " "), ylab = "Normal Density", main= title)
abline(v = mu, col = "red")
abline(v = spread, col = "green")
abline(v = 0, lty = "dashed")

#What is the probability of the home team beating the spread?
spreadprob <- pnorm(spread, mean = mu, sd = sigma, lower.tail = FALSE)
print(paste("The prob of", HomeTm, "beating the spread is", round(spreadprob, 4), sep = " "))

