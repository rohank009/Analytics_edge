## CART for location data
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/location")
boston = read.csv("boston.csv")
str(boston)
summary(boston)

## plotting the location
plot(boston$LON , boston$LAT)
## plotting the points near charles river
points(boston$LON[boston$CHAS==1] ,boston$LAT[boston$CHAS==1],col="blue",pch=19)
points(boston$LON[boston$TRACT==3531] ,boston$LAT[boston$TRACT==3531],col="red",pch=19)

summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55] ,boston$LAT[boston$NOX >= 0.55],col="green",pch=19)
plot(boston$LON , boston$LAT) ## resetting
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2] ,boston$LAT[boston$MEDV >= 21.2],col="red",pch=19)

plot(boston$LAT , boston$MEDV)
plot(boston$LON , boston$MEDV)
latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)
plot(boston$LON , boston$LAT)
points(boston$LON[boston$MEDV >= 21.2] ,boston$LAT[boston$MEDV >= 21.2],col="red",pch=19)
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= 21.2] ,boston$LAT[latlonlm$fitted.values >= 21.2],col="blue",pch="$")

## Regression Trees
latlontree = rpart(MEDV ~ LAT + LON , data=boston)
summary(latlontree)
prp(latlontree)
points(boston$LON[boston$MEDV >= 21.2] ,boston$LAT[boston$MEDV >= 21.2],col="red",pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues >= 21.2] ,boston$LAT[fittedvalues >= 21.2],col="blue",pch="$")

latlontree = rpart(MEDV ~ LAT + LON , data=boston, minbucket=50)
plot(latlontree)
text(latlontree)
plot(boston$LON , boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV >= 21.2] ,boston$LAT[boston$MEDV >= 21.2],col="red",pch=19)

set.seed(123)
split = sample.split(boston$MEDV,SplitRatio=0.7)
train = subset(boston,split==T)
test = subset(boston,split==F)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO, data=train)
summary(linreg)

linreg.predict = predict(linreg , newdata=test)
linreg.sse = sum((linreg.predict - test$MEDV)^2)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO, data=train)
prp(tree)
tree.predict = predict(tree , newdata=test)
tree.sse = sum((tree.predict - test$MEDV)^2)

tr.control = trainControl(method = "CV" , number=10)
cp.grid = expand.grid(.cp=(0:10)*0.001)
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + PTRATIO, data=train , method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)
best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(best.tree , newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)