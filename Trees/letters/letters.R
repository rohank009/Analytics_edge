## Logistic Regression and classification trees
rm(list=ls())
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
setwd("D:/Analytics/Analytics_edge/trees/letters")
letters = read.csv("letters_ABPR.csv")
str(letters)
summary(letters)

## create a new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
spl = sample.split(letters$isB,SplitRatio = 0.5)
train = subset(letters , spl==T)
test = subset(letters , spl==F)
str(train)
str(test)
## accuracy of the baseline method on test set
table(test$isB)
acc = 1175/nrow(test)

## create trees
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

predtest = predict(CARTb, newdata=test , type="class")
table(test$isB , predtest)
acc = (1118+340)/nrow(test)

## building a random forest
set.seed(1000)
letterForest = randomForest(isB ~ . - letter, data=train)
predForest = predict(letterForest , newdata=test)
table(test$isB , predForest)
acc = (1163 + 375)/nrow(test)

## predicting the letter if it is A B P or R
letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter,SplitRatio = 0.5)
train = subset(letters,spl == T)
test = subset(letters,spl == F)

## baseline accuracy
table(test$letter)

## creating the CART model
CARTletter = rpart(letter ~ . -isB , data=train , method = "class")
predletter = predict(CARTletter , newdata=test , type="class")

table(test$letter , predletter)
acc = (348 + 318 + 363 + 340)/nrow(test) ## 0.8786906

## building a random forest
set.seed(1000)
letterForest = randomForest(letter ~ . -isB , data=train)
predForest = predict(letterForest , newdata=test)

table(test$letter , predForest)
acc = (389 + 380 + 393 + 365)/nrow(test)
