## Logistic Regression and classification trees
rm(list=ls())
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/census")
census = read.csv("census.csv")
str(census)
summary(census)

## splitting the data set
set.seed(2000)
spl = sample.split(census$over50k,SplitRatio = 0.6)
train = subset(census,spl == T)
test = subset(census,spl == F)

str(train)
str(test)

## building a log reg mod
logm50k = glm(over50k ~ . , data=train , family = "binomial")
summary(logm50k)

## prediction
predover50k = predict(logm50k , newdata=test , type="response")
table(test$over50k , predover50k > 0.5)
acc = (9051+1888)/nrow(test)

## baseline accuracy for test set
table(test$over50k)
acc = 9713/nrow(test)

## computing area under the curve
## computing AUC
## using ROCR and AUC
ROCRpred = prediction (predover50k , test$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.9061598

## building a tree
CART50k = rpart(over50k ~ ., data=train , method = "class" )
prp(CART50k)

## prediction ,either use type="class" or use > 0.5 like log reg
predCART50k = predict(CART50k , newdata=test, type = "class")
table(test$over50 , predCART50k)
acc = (9243 + 1596)/nrow(test)

## computing area under the curve
## computing AUC
## using ROCR and AUC
## removing type="class" so as to get the auc
predCART50k = predict(CART50k , newdata=test)
ROCRpred = prediction (predCART50k[,2] , test$over50k)
perf = performance(ROCRpred,"tpr","fpr")
plot(perf)

auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.8470256

## randomforest on 2000 random observations from training set
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
over50kForest = randomForest(over50k ~ . , data=trainSmall)
predictForest = predict(over50kForest , newdata=test)
table(test$over50k , predictForest)
acc = (9633 + 859)/nrow(test)

## to see which variables were used in random forest
vu = varUsed(over50kForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(over50kForest$forest$xlevels[vusorted$ix]))

## measuring the impurity reduction
varImpPlot(over50kForest)

## selecting cp value
set.seed(2)
tr.control = trainControl(method = "CV" , number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data=train , method = "rpart" , trControl = tr.control , tuneGrid = cartGrid)

## fitting CART model using the cp value
CARTcp = rpart(over50k ~ . , data=train, method = "class",cp=0.002)
predictcp = predict(CARTcp , newdata=test , type="class")
table(test$over50k , predictcp)
acc = (9178 + 1838)/nrow(test)
prp(CARTcp)
