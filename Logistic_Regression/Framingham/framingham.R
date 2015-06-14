## Quality care logistic Regression
library("caTools")
library("ROCR")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/Framingham")
framingham = read.csv("framingham.csv")
str(framingham)

set.seed(1000)
split = sample.split(framingham$TenYearCHD,SplitRatio = 0.65)
train = subset(framingham,split == T)
test = subset(framingham,split == F)
str(train)
str(test)

## create logistic model
framinghamLog = glm(TenYearCHD ~ ., data=train, family = binomial)
summary(framinghamLog)

## predicting on the test set
predictTest = predict(framinghamLog , type="response" , newdata=test)
summary(predictTest)

## creating a confusion matrix
table(test$TenYearCHD , predictTest > 0.5)
## accuracy of the model
(1069 + 11)/(1069 + 6 + 11 + 187) ## = 0.8483896
## baseline model accuracy that will always predict no CHD
(1069 + 6)/(1069 + 6 + 11 + 187) ## = 0.8444619

## using ROCR and AUC
ROCRpred = prediction (predictTest , test$TenYearCHD)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.7421095