# Election Forecasting
library(ggplot2)
library(ggmap)
library(maps)

setwd("D:/Analytics/Analytics_edge/Visualization/election")
statesMap = map_data("state")
str(statesMap)

## One of the variables, group, defines the different shapes or polygons on the map. Sometimes a state may have multiple groups, for example, if it includes islands
## The variable "order" defines the order to connect the points within each group, and the variable "region" gives the name of the state.

## drawing US map
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv")
str(polling)
pollingtrain = subset(polling , polling$Year < 2009)
pollingtest = subset(polling , polling$Year > 2008)

## creating log model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=pollingtrain, family="binomial")
testpred = predict(mod2 , newdata=pollingtest , type = "response")

TestPredictionBinary = as.numeric(testpred > 0.5) ## pred for republican/democrat

predictionDataFrame = data.frame(testpred, TestPredictionBinary, pollingtest$State)

str(predictionDataFrame)
table(predictionDataFrame$TestPredictionBinary) ## having prob as 1
mean(predictionDataFrame$testpred)

## merging statesmap and above data frame
predictionDataFrame$region = tolower(predictionDataFrame$pollingtest.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

## make sure observations are in order
predictionMap = predictionMap[order(predictionMap$order),]

## coloring the states by prediction
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

## modify the coloring

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

## changing the fill
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testpred))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

## using linetype
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testpred))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

## using size
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testpred))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

## using alpha
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testpred))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")