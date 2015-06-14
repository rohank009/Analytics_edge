## Linear Regression for Basketball 
setwd("D:/Analytics/Analytics_edge/Linear_Regression/NBA")
nba=read.csv("NBA_train.csv")
str(nba)
summary(basketball)

table(nba$W,nba$Playoffs)

nba$ptsdiff=nba$PTS - nba$oppPTS

plot(nba$ptsdiff,nba$W)

## create a linear model
winsreg = lm(W ~ ptsdiff, data=nba)
summary(winsreg)

## regression equation
w = 41 + 0.0326 * (ptsdiff)

## a team has to win at least 42 games to make it to the payoffs
42 = 41 + 0.0326 * (ptsdiff)
ptsdiff = (42 - 41) / 0.0326 = 30.67485 = 31

## equation for points scored
pointsreg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=nba)
summary(pointsreg)

SSE = sum(pointsreg$residuals^2) ## sum of squares
RMSE = sqrt(SSE/nrow(nba)) ## root mean squared error

## avg number of points in a season
mean(nba$PTS)

## removing insignificant variables
pointsreg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=nba)
summary(pointsreg2)


## removing insignificant variables
pointsreg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=nba)
summary(pointsreg3)

## removing insignificant variables
pointsreg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=nba)
summary(pointsreg4)

## to check if the error has not been inflated
SSE4 = sum(pointsreg4$residuals^2) ## sum of squares
RMSE4 = sqrt(SSE/nrow(nba)) ## root mean squared error


## make predictions
nba_test = read.csv("NBA_test.csv")
str(nba_test)

pointsprediction = predict(pointsreg4 , newdata=nba_test)
SSE = sum((pointsprediction - nba_test$PTS)^2) 
SST = sum ((nba_test$PTS - mean(nba$PTS))^2)
r_sq = 1-SSE/SST
rmse = sqrt(SSE/nrow(nba_test))