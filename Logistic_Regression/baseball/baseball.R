## baseball
library("caTools")
library("ROCR")
library("mice")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/baseball")
baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)

## Though the dataset contains data from 1962 until 2012, we removed several years with shorter-than-usual seasons. Using the table() function, identify the total number of years included in this dataset.
table(baseball$Year)

## Because we're only analyzing teams that made the playoffs, use the subset() function to replace baseball with a data frame limited to teams that made the playoffs (so your subsetted data frame should still be called "baseball"). How many team/year pairs are included in the new dataset?
baseball = subset(baseball , Playoffs == 1)
str(baseball)

## Through the years, different numbers of teams have been invited to the playoffs. Which of the following has been the number of teams making the playoffs in some season? Select all that apply.
table(baseball$Year)

## adding a predictor number of competitors
PlayoffTable = table(baseball$Year)
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

## Add the NumCompetitors variable to your baseball data frame. How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
table(baseball$NumCompetitors==8)

## adding worldseries variable
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)

## How many observations do we have in our dataset where a team did NOT win the World Series?
table(baseball$WorldSeries != 1)

## build 12 models to select our bivariate variable
logm_bi = glm(WorldSeries ~ RS, data=baseball , family=binomial)
summary(logm_bi)


## building a bivariate model combining all the significant variables
logm = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball , family=binomial)
summary(logm)

## correlation
cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])

## building 6 + 4 bivariate models
logm1 = glm(WorldSeries ~ Year , data=baseball , family=binomial)
summary(logm1) ## AIC=232.35
logm2 = glm(WorldSeries ~ RA , data=baseball , family=binomial)
summary(logm2) ## AIC=237.88
logm3 = glm(WorldSeries ~ RankSeason , data=baseball , family=binomial)
summary(logm3) ## AIC=238.75
logm4 = glm(WorldSeries ~ NumCompetitors , data=baseball , family=binomial)
summary(logm4) ## AIC=230.96
logm5 = glm(WorldSeries ~ Year + RA , data=baseball , family=binomial)
summary(logm5) ## AIC=233.88
logm6 = glm(WorldSeries ~ Year + RankSeason , data=baseball , family=binomial)
summary(logm6) ## AIC=233.55
logm7 = glm(WorldSeries ~ Year + NumCompetitors , data=baseball , family=binomial)
summary(logm7) ## AIC=232.9
logm8 = glm(WorldSeries ~ RA + RankSeason , data=baseball , family=binomial)
summary(logm8) ## AIC=238.22
logm9 = glm(WorldSeries ~ RA + NumCompetitors , data=baseball , family=binomial)
summary(logm9) ## AIC=232.74
logm10 = glm(WorldSeries ~ RankSeason + NumCompetitors , data=baseball , family=binomial)
summary(logm10) ## AIC=232.52