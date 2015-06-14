## Stocks

rm(list=ls())
library(flexclust) ## required for prediction
library(caret)
library(caTools)
setwd("D:/Analytics/Analytics_edge/clustering/stocks")
stocks = read.csv("StocksCluster.csv", header = TRUE)
str(stocks)

table(stocks$PositiveDec)

## maximum correlation between variables
cor(stocks)

## Which month (from January through November) has the largest mean return across all observations in the dataset?
max(colMeans(stocks[,1:11]))
min(colMeans(stocks[,1:11]))

## splitting the data
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

## training a log reg mod

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)

PredictTrain = predict(StocksModel, type="response")

table(stocksTrain$PositiveDec, PredictTrain > 0.5)
acc = (3640 + 990)/nrow(stocksTrain)

predictStockstest = predict(StocksModel,newdata = stocksTest , type = "response")
table(stocksTest$PositiveDec , predictStockstest>0.5)
acctest = (417+1553)/nrow(stocksTest)

## test accuracy for baseline model
table (stocksTest$PositiveDec)
accbase = (1897/nrow(stocksTest))

## removing the dependent variable so as to do clustering

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

## normalize data, subtract from mean and divide by standard deviation
preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain)
summary(normTest)

## Since the mean for training set is greater than test set hence mean for returnjan is negative for test set
## since normalization is done using training set
mean(stocksTrain$ReturnJan)
mean(stocksTest$ReturnJan)

set.seed(144)
k=3
km = kmeans(normTrain, centers = k)
kmtest = kmeans(normTest, centers = k)
## prediction
km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

stocksTraincluster = split(stocksTrain , clusterTrain)
stocksTestcluster = split(stocksTest , clusterTest)

## get means for all three clusters
lapply(stocksTraincluster, colMeans)

## creating log reg mod for three clusters
StockModel1 = glm(PositiveDec ~ . , data=stocksTraincluster[[1]],family = "binomial")
StockModel2 = glm(PositiveDec ~ . , data=stocksTraincluster[[2]],family = "binomial")
StockModel3 = glm(PositiveDec ~ . , data=stocksTraincluster[[3]],family = "binomial")

summary(StockModel1)
summary(StockModel2)
summary(StockModel3)

## prediction on the test set
predicttest1 = predict(StockModel1 , newdata = stocksTestcluster[[1]] , type="response")
predicttest2 = predict(StockModel2 , newdata = stocksTestcluster[[2]] , type="response")
predicttest3 = predict(StockModel3 , newdata = stocksTestcluster[[3]] , type="response")

## computing accuracies
table(stocksTestcluster[[1]]$PositiveDec , predicttest1>0.5)
acc1 = (38+774)/nrow(stocksTestcluster[[1]])

table(stocksTestcluster[[2]]$PositiveDec , predicttest2>0.5)
acc2 = (388+757)/nrow(stocksTestcluster[[2]])

table(stocksTestcluster[[3]]$PositiveDec , predicttest3>0.5)
acc3 = (49+13)/nrow(stocksTestcluster[[3]])

AllPredictions = c(predicttest1, predicttest2, predicttest3)
AllOutcomes = c(stocksTestcluster[[1]]$PositiveDec, stocksTestcluster[[2]]$PositiveDec, stocksTestcluster[[3]]$PositiveDec)

table(AllOutcomes , AllPredictions>0.5)
overacc = (467+1544)/length(AllOutcomes)