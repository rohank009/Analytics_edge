## State Data
##library(zoo)

setwd("D:/Analytics/Analytics_edge/Linear_Regression/statedata")
##state = read.csv("statedata.csv")

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
summary(statedata)

## plot the latitude and longitude
plot(statedata$x,statedata$y)

## Using the tapply command, determine which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad,statedata$state.region,mean)

## Now, make a boxplot of the murder rate by region (for more information about creating boxplots in R, type ?boxplot in your console).
## Which region has the highest median murder rate?
boxplot(statedata$Murder ~ statedata$state.region ,data=statedata)

## You should see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to? (Hint: There are many ways to find the answer to this question, 
## but one way is to use the subset command to only look at the Northeast data.)
state_outlier = subset(statedata, statedata$state.region=="Northeast")

## We would like to build a model to predict life expectancy by state using the state statistics we have in our dataset.
## Build the model with all potential variables included (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area). Note that you should use the variable "Area" in your model, NOT the variable "state.area".
## What is the coefficient for "Income" in your linear regression model?
lmLifeexp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(lmLifeexp)

## Now plot a graph of life expectancy vs. income using the command:
## Visually observe the plot. What appears to be the relationship?
plot(statedata$Income, statedata$Life.Exp)

## You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
lmLifeexp1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=statedata)
summary(lmLifeexp1)

lmLifeexp2 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data=statedata)
summary(lmLifeexp2)

lmLifeexp3 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(lmLifeexp3)

lm_step = step(lmLifeexp)

## Take a look at the vector of predictions by using the predict function (since we are just looking at predictions on the training set, you don't need to pass a "newdata" argument to the predict function).
## Which state do we predict to have the lowest life expectancy? (Hint: use the sort function)
predictLifexp = predict(lmLifeexp3)
sort(predictLifexp)

## Which state actually has the lowest life expectancy? (Hint: use the which.min function)
statedata[which.min(statedata$Life.Exp),]

## Which state do we predict to have the highest life expectancy? (Hint: use the sort function)
predictLifexp = predict(lmLifeexp3)
sort(predictLifexp)

## Which state actually has the highest life expectancy? (Hint: use the which.min function)
statedata[which.max(statedata$Life.Exp),]

## Take a look at the vector of residuals (the difference between the predicted and actual values).
## For which state do we make the smallest absolute error?
sort(abs(lmLifeexp3$residuals))

## For which state do we make the largest absolute error?
sort(abs(lmLifeexp3$residuals))

## CART creation
CARTstate = rpart(statedata$Life.Exp ~ . , data=statedata)