## Quality care logistic Regression
library("caTools")
library("ROCR")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/quality_care")
quality = read.csv("quality.csv")
str(quality)
summary(quality)

table(quality$PoorCare)
set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)
qualityTrain = subset(quality,split == T)
qualityTest = subset(quality,split == F)

## building the logistic regression
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics , data = qualityTrain, family=binomial)
summary(QualityLog)

predictTrain = predict(QualityLog , type = "response")
summary(predictTrain)

## average prediction for the outcome
tapply(predictTrain , qualityTrain$PoorCare , mean)


## building the logistic regression to include StartedOnCombination
QualityLog1 = glm(PoorCare ~ ProviderCount + StartedOnCombination , data = qualityTrain, family=binomial)
summary(QualityLog1)

## Confusion matrix
table(qualityTrain$PoorCare , predictTrain > 0.5)

sensitivity = 10/(10+15)  ## TP/TP + FN
specificity = 70/(70 + 4)  ## TN/TN + FP

## Confusion matrix increasing the threshold
table(qualityTrain$PoorCare , predictTrain > 0.7)

sensitivity = 8/(8+17)  ## TP/TP + FN
specificity = 73/(73 + 1)  ## TN/TN + FP

## Confusion matrix decreasing the threshold
table(qualityTrain$PoorCare , predictTrain > 0.2)

sensitivity = 16/(9+16)  ## TP/TP + FN
specificity = 54/(54 + 20)  ## TN/TN + FP

## plotting the ROC curve
# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

--------------------------- TEST SET ---------------------------------------------

## predicting the test sample
predictTest = predict(QualityLog , type="response", newdata=qualityTest)

## Confusion matrix for test sample
table(qualityTest$PoorCare , predictTest > 0.3)

## plotting the ROC curve
# Prediction function
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

# Performance function
ROCRperftest = performance(ROCRpredTest, "tpr", "fpr")

## computing the area under curve (auc)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

# Plot ROC curve
plot(ROCRperftest)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
