## Forecasting Elantra sales


setwd("D:/Analytics/Analytics_edge/Linear_Regression/Elantra")
elantra = read.csv("elantra.csv")

elantraTrain=subset(elantra,elantra$Year < 2013)
elantraTest=subset(elantra,elantra$Year > 2012)

str(elantraTrain)
summary(elantraTrain)

## building a linear model to predict monthly sales
lmSales = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=elantraTrain)
summary(lmSales)

## building a linear model to predict monthly sales including month variable
lmSales1 = lm(ElantraSales ~ Month + Unemployment + Queries + CPI_energy + CPI_all, data=elantraTrain)
summary(lmSales1)

## In the new model, given two monthly periods that are otherwise identical in Unemployment, CPI_all, CPI_energy and Queries, what is the absolute difference in predicted Elantra sales given that one period is in January and one is in March?
## The coefficient for Month is 110.69 (look at the summary output of the model). For the first question, January is coded numerically as 1, while March is coded numerically as 3; the difference in the prediction is therefore

110.69 * (3 - 1) = 110.69 * 2 = 221.38

##For the second question, May is numerically coded as 5, while January is 1, so the difference in predicted sales is

110.69 * (5 - 1) = 110.69 * 4 = 442.76

##create month as a factor variable
elantraTrain$MonthFac = as.factor(elantraTrain$Month)
str(elantraTrain)

## building a linear model to predict monthly sales including monthfac variable
lmSales2 = lm(ElantraSales ~ MonthFac + Unemployment + Queries + CPI_energy + CPI_all, data=elantraTrain)
summary(lmSales2)

## Which of the following variables is CPI_energy highly correlated with? Select all that apply.
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

## Which of the following variables is Queries highly correlated with?
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

## building a linear model to predict monthly sales by removing the insignificant variables
lmSales3 = lm(ElantraSales ~ MonthFac + Unemployment + CPI_energy + CPI_all, data=elantraTrain)
summary(lmSales3)

## Prediction in test set
elantraTest$MonthFac = as.factor(elantraTest$Month)
predTest = predict(lmSales3 , newdata=elantraTest)
summary(predTest)
SSE = sum((predTest - elantraTest$ElantraSales)^2)
SST = sum((elantraTest$ElantraSales - mean(elantraTrain$ElantraSales))^2)
r_sq = 1 - (SSE/SST)

## What is the largest absolute error that we make in our test set predictions?
which.max(abs(predTest - elantraTest$ElantraSales))