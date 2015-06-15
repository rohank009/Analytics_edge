## CART for court cases
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/courtcases")
stevens = read.csv("stevens.csv")
str(stevens)
summary(stevens)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio=0.7)
train = subset(stevens,spl==T)
test = subset(stevens,spl==F)
str(train)
str(test)

## building the tree
stevenstree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , method = "class", minbucket=25)
prp(stevenstree)

## predicting on test set
predictcart = predict(stevenstree,newdata=test , type="class")

table(test$Reverse,predictcart)
accuracy=(41+71)/(41+71+22+36)

## plotting the roc curve
predictroc = predict(stevenstree,newdata=test)
pred = prediction(predictroc[,2],test$Reverse)
perf = performance(pred,"tpr","fpr")
plot(perf)
auc = as.numeric(performance(pred, "auc")@y.values)

## building the tree with minbucket as 5
stevenstree1 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , method = "class", minbucket=5)
prp(stevenstree1)

## building the tree with minbucket as 100
stevenstree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , method = "class", minbucket=100)
prp(stevenstree2)

## building randomforest
stevensforest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , nodesize=25 , ntree=200)

## converting the predict variable to FACTOR for random forest classification
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
set.seed(200)
stevensforest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , nodesize=25 , ntree=200)

## predicting on test set
predictforest = predict(stevensforest , newdata=test)
table(test$Reverse,predictforest)
accuracy = (41 + 75)/(41+75+36+18)

## cross validation
numfolds = trainControl(method="CV" , number=10)
cpgrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst , data=train , method="rpart", trControl = numfolds , tuneGrid = cpgrid)
## cp received as 0.19
## using this cp for our cart model
stevenstreecv = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,data=train , method="class",cp=0.19)
predictcv = predict(stevenstreecv , newdata=test , type="class")
table(test$Reverse,predictcv)
accuracy = (59 + 64)/(59+64+18+29) ## 0.7235294

## plotting the tree
prp(stevenstreecv)