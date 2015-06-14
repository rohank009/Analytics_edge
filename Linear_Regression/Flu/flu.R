## Reading Test Scores
library(zoo)

setwd("D:/Analytics/Analytics_edge/Linear_Regression/flu")
flu_train=read.csv("FluTrain.csv")
str(flu_train)
summary(flu_train)
flu_test=read.csv("FluTest.csv")

## Looking at the time period 2004-2011, which week corresponds to the highest percentage of ILI-related physician visits? 
## Select the day of the month corresponding to the start of this week.
flu_train$Week[which.max(flu_train$ILI)]

## Which week corresponds to the highest percentage of ILI-related query fraction?
flu_train$Week[which.max(flu_train$Queries)]

## Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(flu_train$ILI)
hist(log(flu_train$ILI))

## Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(log(flu_train$ILI),flu_train$Queries)

## Let's call the regression model from the previous problem (Problem 2.1) FluTrend1 and run it in R. Hint: to take the logarithm of a variable Var in a regression equation, 
## you simply use log(Var) when specifying the formula to the lm() function.
lm_ili = lm(log(ILI) ~ Queries , data=flu_train)
summary(lm_ili)

## For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between the independent and the dependent variables. What is the relationship we infer from our problem? 
## (Don't forget that you can use the cor function to compute the correlation between two variables.)
cor(log(flu_train$ILI),flu_train$Queries)

## prediction
pred_test = exp(predict(lm_ili, newdata=flu_test))

## What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? (HINT: You can either just output FluTest$Week to find which element corresponds to March 11, 2012, or you can use the "which" function in R. 
## To learn more about the which function, type ?which in your R console.)

which(flu_test$Week == "2012-03-11 - 2012-03-17")
pred_test[11]

## What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? Note that the relative error is calculated as
## (Observed ILI - Estimated ILI)/Observed ILI
rel_error= (flu_test$ILI[11] - pred_test[11])/flu_test$ILI[11]

## What is the Root Mean Square Error (RMSE) between our estimates and the actual observations 
## for the percentage of ILI-related physician visits, on the test set?
SSE = sum((pred_test - flu_test$ILI)^2)
RMSE = sqrt(SSE/nrow(flu_test))

## How many values are missing in the new ILILag2 variable?
ILILag2 = lag(zoo(flu_train$ILI), -2, na.pad=TRUE)
flu_train$ILILag2 = coredata(ILILag2)

summary(flu_train)

## Use the plot() function to plot the log of ILILag2 against the log of ILI. 
## Which best describes the relationship between these two variables?
plot(log(flu_train$ILI), log(flu_train$ILILag2))

## Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable. 
## Call this model FluTrend2.
flutrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = flu_train)
summary(flutrend2)

## Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. 
## How many missing values are there in this new variable?
ILILagtest = lag(zoo(flu_test$ILI), -2, na.pad=T)
flu_test$ILILag2 = coredata(ILILagtest)
summary(flu_test)

## Fill in the missing values for ILILag2 in FluTest. In terms of syntax, you could set the value of ILILag2 in row "x" of the FluTest data frame to the value of ILI in row "y" of the FluTrain data frame with "FluTest$ILILag2[x] = FluTrain$ILI[y]".
flu_test$ILILag2[1] = flu_train$ILI[nrow(flu_train)-1] ## for the first row of test dataset
flu_test$ILILag2[2] = flu_train$ILI[nrow(flu_train)] ## for the second row of test dataset

## Obtain test set predictions of the ILI variable from the FluTrend2 model, again remembering to call the exp() function on the result of the predict() function to obtain predictions for ILI instead of log(ILI).
## What is the test-set RMSE of the FluTrend2 model?
predict_test = exp(predict(flutrend2 , newdata=flu_test))
summary(predict_test)
SSE = sum((predict_test - flu_test$ILI)^2)
RMSE = sqrt(SSE/nrow(flu_test))