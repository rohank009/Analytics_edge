## Reading Test Scores
setwd("D:/Analytics/Analytics_edge/Linear_Regression/pisa")
pisa_train=read.csv("pisa2009train.csv")
pisa_test=read.csv("pisa2009test.csv")
str(pisa_train)

## Using tapply() on pisaTrain, what is the average reading test score of males/females?
a=tapply(pisa_train$readingScore,pisa_train$male==1,mean)

## Which variables are missing data in at least one observation in the training set? Select all that apply.
summary(pisa_train)

pisa_train = na.omit(pisa_train)
pisa_test = na.omit(pisa_test)

## How many observations are now in the training/test set?
str(pisa_train)
str(pisa_test)

## setting up the reference level for race as WHITE
pisa_train$raceeth = relevel(pisa_train$raceeth, "White")
pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

## create a linear model to predict the reading score for training set
lmscore = lm(pisa_train$readingScore ~ .,data=pisa_train)
summary(lmscore)

## find RMSE
SSE = sum(lmscore$residuals^2)
RMSE = sqrt(SSE/nrow(pisa_train)) ## root mean squared error

## Consider two students A and B. They have all variable values the same, except that student A is in grade 11 and student B is in grade 9. 
## What is the predicted reading score of student A minus the predicted reading score of student B?
y = beta0 + beta1*grade
readingscore_A = 143.766333 + (29.542707)*11
readingscore_B = 143.766333 + (29.542707)*9
readingscore_A - readingscore_B

## Using the "predict" function and supplying the "newdata" argument, use the lmScore model to predict the reading scores of students in pisaTest. Call this vector of predictions "predTest". Do not change the variables in the model (for example, do not remove variables that we found were not significant in the previous part of this problem). Use the summary function to describe the test set predictions.
## What is the range between the maximum and minimum predicted reading score on the test set?

predscore = predict(lmscore , newdata=pisa_test)
summary(predscore)

## What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE = sum((pisa_test$readingScore - predscore)^2)
RMSE = sqrt(SSE/nrow(pisa_test)) ## root mean squared error

## What is the predicted test score used in the baseline model? 
## Remember to compute this value using the training set and not the test set.
pred_test_score_base=mean(pisa_train$readingScore)

## What is the sum of squared errors of the baseline model on the testing set? HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
SST = sum((pisa_test$readingScore - mean(pisa_train$readingScore))^2 )

## What is the test-set R-squared value of lmScore?
r_sq = 1-(SSE/SST)