## Linear Regression for Baseball in R Video
setwd("D:/Analytics/Analytics_edge/Linear_Regression")
baseball=read.csv("baseball.csv")
str(baseball)
summary(baseball)

## taking data less than 2002
moneyball = subset(baseball,Year < 2002)
str(moneyball)
## finding the difference between runs scored and runs allowed
moneyball$RD = moneyball$RS - moneyball$RA
## Plot
plot(moneyball$RD , moneyball$W) 
## Linear Model
winsreg = lm(W ~ RD , data=moneyball)
summary(winsreg)

## If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?

y = 80.881375 + 0.105766(RD) ## y = beta0 + beta1(x)
y = 80.881375 + 0.105766 * (713 - 614)
y = 91.35221

## linear model for runs scored
runsreg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(runsreg)

## remove BA from the model
runsreg = lm(RS ~ OBP + SLG , data=moneyball)
summary(runsreg)

## If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?
RS = -804.63 + 2737.77 * (0.311) + 1584.91 * (0.405) # y = beta0 + beta1*x1 + beta2 * x2

## linear model for runs allowed
runsallreg = lm(RA ~ OOBP + OSLG , data=moneyball)
summary(runsallreg)

## If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, 
## how many runs do we expect the team to allow?
RA = -837.38 + 2913.60 * (0.297) + 1514.29 * (0.370) # y = beta0 + beta1*x1 + beta2 * x2

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

## What is the correlation between teamRank and wins2012?
cor(teamRank,wins2012)
cor(teamRank,wins2013)
