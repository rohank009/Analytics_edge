## Logistic Regression and classification trees
rm(list=ls())
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/voting")
vote = read.csv("gerber.csv")
str(vote)
summary(vote)

## We will first get familiar with the data. 
## Load the CSV file gerber.csv into R. 
## What proportion of people in this dataset voted in this election?
table(vote$voting) ## 0.3158996

## Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
table(vote$voting == 1 & vote$civicduty == 1)
table(vote$voting == 1 & vote$hawthorne == 1)
table(vote$voting == 1 & vote$self == 1)
table(vote$voting == 1 & vote$neighbors == 1)

## logistic regression model
logmVote = glm(voting ~ civicduty + hawthorne + neighbors + self,data=vote,family=binomial)
summary(logmVote)
predVote1 = predict(logmVote , type="response")

##Using a threshold of 0.3, what is the accuracy of the logistic regression model? 
table(vote$voting , predVote1 > 0.3)
accuracy = (134513 + 51966)/nrow(vote)

##Using a threshold of 0.5, what is the accuracy of the logistic regression model? 
table(vote$voting , predVote1 > 0.5)
accuracy = (235388)/nrow(vote)

## computing AUC
## using ROCR and AUC
ROCRpred = prediction (predVote1 , vote$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.5308461

## creating TREES using CART model
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ control + sex + civicduty + hawthorne + self + neighbors, data=vote, cp=0.0)
prp(CARTmodel3)

## create trees only with the control group
CARTmodel4 = rpart(voting ~ control , data=vote, cp=0.0)
prp(CARTmodel4,digits=6)

CARTmodel5 = rpart(voting ~ control + sex , data=vote, cp=0.0)
prp(CARTmodel5,digits=6)

## creating logistic model for control and sex
logmsex = glm(voting ~ control + sex , data=vote , family = "binomial")
summary(logmsex)
## If you look at the summary of the model, you can see that the coefficient for the "sex" variable is -0.055791. This means that women are less likely to vote, since women have a larger value in the sex variable, and a negative coefficient means that larger values are predictive of 0.

## to help log m to consider both control and sex
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logmsex, newdata=Possibilities, type="response")
## abs diff bet CART and logm is 0.290456 - 0.2908065

## adding the sex and control variable together in the log reg m
logmsex1 = glm(voting ~ control + sex + sex:control, data=vote , family = "binomial")
summary(logmsex1) ## This coefficient is negative, so that means that a value of 1 in this variable decreases the chance of voting. This variable will have variable 1 if the person is a woman and in the control group.
predict(logmsex1, newdata=Possibilities, type="response")
## abs diff bet CART and logm is 0.290456 - 0.2904558