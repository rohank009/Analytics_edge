## Loan Repayment
library("caTools")
library("ROCR")
library("mice")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/loans")
loans = read.csv("loans.csv")
str(loans)
summary(loans)

## What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
table(loans$not.fully.paid)

## populating missing data
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | 
          is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loans)

## splitting the dataset into Train and Test
split = sample.split(loans$not.fully.paid,SplitRatio=0.7)
train = subset(loans,split==T)
test = subset(loans,split==F)
str(train)
str(test)

# training the logistic model
logm = glm(not.fully.paid ~ .,data=train,family=binomial)
summary(logm)

## Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic regression model, and define Logit(B) similarly for loan B. 
## What is the value of Logit(A) - Logit(B)?

logit_diff = ((-7.983e-03 * 700)) - ((-7.983e-03 * 710))


##Now, let O(A) be the odds of loan A not being paid back in full, according to our logistic regression model, and define O(B) similarly for loan B. What is the value of O(A)/O(B)? (HINT: Use the mathematical rule that exp(A + B + C) = exp(A)*exp(B)*exp(C). 
##Also, remember that exp() is the exponential function in R.)


## predicting on the test set
predicted.risk = predict(logm, newdata=test, type="response")
test$predicted.risk = predicted.risk
table(test$not.fully.paid , predicted.risk > 0.5)

## overall model accuracy
(2406 + 21)/(2406+21+7+439)

## accuracy of the baseline model
2413/(2413+439+21)

## computing AUC
## using ROCR and AUC
ROCRpred = prediction (predicted.risk , test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.6744725


## building a bivariate logistic reg using int.rate variable
logm_intrate = glm(not.fully.paid ~ int.rate , data=train , family=binomial)
summary(logm_intrate)

## prediction
predtest_intrate = predict(logm_intrate,newdata=test,type="response")
## Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
summary(predtest_intrate) ##0.4413187
## With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
summary(predtest_intrate) ##0.4413187

## computing AUC
## using ROCR and AUC
ROCRpred = prediction (predtest_intrate , test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.6136025

## In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, meaning t=3 in our calculations. Enter the following commands in your R console to create this new variable:
## creating a new variable to calculate the compounded interest
test$profit = exp(test$int.rate*3) - 1 
test$profit[test$not.fully.paid == 1] = -1

## What is the maximum profit of a $10 investment in any loan in the testing set (do not include the $ sign in your answer)?
max(test$profit)

## creating a data frame with loans having interest rate > 15
highinterest = subset(test, int.rate >= 0.15)
summary(highinterest)
## What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
mean(highinterest$profit) ## 0.2566929
## What proportion of the high-interest loans were not paid back in full?
table(highinterest$not.fully.paid) ## 95/(311+95)

## Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order and selecting the 100th element of this sorted list. Find the highest predicted risk that we will include by typing the following command into your R console:
cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]

## Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed. Check to make sure you have selected 100 loans for investment.
selectedloans = subset(highinterest ,  predicted.risk <= cutoff)

## What is the profit of the investor, who invested $1 in each of these 100 loans (do not include the $ sign in your answer)?
sum(selectedloans$profit)

## How many of 100 selected loans were not paid back in full?
table(selectedloans$not.fully.paid)