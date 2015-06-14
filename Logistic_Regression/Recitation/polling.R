## Recitation Polling data
library("caTools")
library("ROCR")
library("mice")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/Recitation")
polling = read.csv("PollingData.csv")
str(polling)

## to substitute for missing values
table(polling$Year)
summary(polling)

simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple)) ## running imputations to fill in missing variables
summary(imputed)

## copyuing the variables back to polling
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

## creating dataset
Train = subset(polling , Year == 2004 | Year == 2008)
Test = subset(polling , Year == 2012)
table(Train$Republican)

## baselining based on Rasmussen variable value
table(sign(Train$Rasmussen))
## comparing the training set's outcome against the Rasmussen
table(Train$Republican,sign(Train$Rasmussen))

## finding correlation
cor(Train[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

## building model
mod1 = glm(Republican ~ PropR , data = Train, family = "binomial")
summary(mod1)
pred1 = predict(mod1 , type="response")
table(Train$Republican , (pred1 >= 0.5))

## to improve performance add another variable
mod2 = glm(Republican ~ PropR + DiffCount + SurveyUSA , data = Train, family = "binomial")
summary(mod2)
pred2 = predict(mod2 , type="response")
table(Train$Republican , (pred2 >= 0.5))

## predicting on test set
table(Test$Republican , sign(Test$Rasmussen))
predTest = predict(mod2 , newdata=Test, type="response")
table(Test$Republican , (predTest >= 0.5))

## find out the mistake that we made in the model prediction
subset(Test , predTest >= 0.5 & Republican == 0 )