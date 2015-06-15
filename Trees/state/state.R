## Logistic Regression and classification trees
rm(list=ls())
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/state")
state = read.csv("statedataSimple.csv")
str(state)
summary(state)

## building the linear model
linm = lm(Life.Exp ~ .,data=state)
summary(linm)
predictlinm = predict(linm)
SSE = sum((state$Life.Exp - predictlinm)^2)

## building the lin mod again using Population, Murder, Frost, and HS.Grad
linm1 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad  ,data=state)
summary(linm1)
predictlinm1 = predict(linm1)
SSE = sum((state$Life.Exp - predictlinm1)^2)

## CART
CARTstate = rpart(state$Life.Exp ~ . , data=state)
prp(CARTstate)

## prediction
CARTpredict = predict(CARTstate)

SSE_CART = sum((state$Life.Exp - CARTpredict)^2)

## setting the minbucket param to 5
CARTstate = rpart(state$Life.Exp ~ . , data=state,minbucket=5)
prp(CARTstate)

CARTpredict = predict(CARTstate)
SSE_CART = sum((state$Life.Exp - CARTpredict)^2)

## taking only AREA variable for CART
CARTarea = rpart(state$Life.Exp ~ Area , data=state,minbucket=1)
prp(CARTarea)

CARTpredict = predict(CARTarea)
SSE_CART = sum((state$Life.Exp - CARTpredict)^2)

set.seed(111)

tr.control = trainControl(method = "cv", number = 10)
cp.grid =  expand.grid(.cp = seq(0.01, 0.5, 0.01) )
tr = train(state$Life.Exp ~ . ,data=state , method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)

CARTstate = rpart(state$Life.Exp ~ . , data=state,cp=0.12)
prp(CARTstate)

CARTpredict = predict(CARTstate)
SSE_CART = sum((state$Life.Exp - CARTpredict)^2)

##----------
set.seed(111)

tr.control = trainControl(method = "cv", number = 10)
cp.grid =  expand.grid(.cp = seq(0.01, 0.5, 0.01) )
tr = train(state$Life.Exp ~ Area ,data=state , method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)

CARTstate = rpart(state$Life.Exp ~ Area , data=state,cp=0.02)
prp(CARTstate)

CARTpredict = predict(CARTstate)
SSE_CART = sum((state$Life.Exp - CARTpredict)^2)