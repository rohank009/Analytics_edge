## Linear Regression in R Video
setwd("D:/Analytics/Analytics_edge/Linear_Regression")
wine=read.csv("wine.csv")
str(wine)
summary(wine)
## Creating the linear model
model1=lm(Price ~ AGST , data=wine)
summary(model1)
model1$residuals
## calculating sum of squared errors
SSE = sum(model1$residuals^2)

## adding a new variable to the model
model2=lm(Price ~ AGST+HarvestRain , data=wine)
summary(model2)
model2$residuals
## calculating sum of squared errors
SSE = sum(model2$residuals^2)

## adding all variables to the model
model3=lm(Price ~ AGST+HarvestRain+WinterRain+Age+FrancePop , data=wine)
summary(model3)
model3$residuals
## calculating sum of squared errors
SSE = sum(model3$residuals^2)

## adding harvestrain and winterrain variables to the model
model4=lm(Price ~ HarvestRain+WinterRain , data=wine)
summary(model4)
model4$residuals
## calculating sum of squared errors
SSE = sum(model4$residuals^2)

## Removing france population from the model
model5=lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model5)

## finding correlation between variables
cor(wine$WinterRain,wine$Price)
cor(wine$FrancePop,wine$Age)
cor(wine)

## Removing france population and age from the model
model6=lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model6)

wine_test=read.csv("wine_test.csv")
str(wine_test)
## preparing model on wine_test
model5=lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model5)

## predicting the price for wine_test using omdel5
predict_test= predict(model5,newdata=wine_test)
predict_test
SSE=sum((wine_test$Price - predict_test)^2) ## difference between test set and predicted set
SST = sum((wine_test$Price - mean(wine$Price))^2) ## difference between test set and baseline set

1-(SSE/SST)