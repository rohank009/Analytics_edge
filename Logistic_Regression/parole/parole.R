## songs
library("caTools")
library("ROCR")
library("mice")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/parole")
parole = read.csv("parole.csv")
str(parole)
summary(parole)

## How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

## How does the output of summary() change for a factor variable as compared to a numerical variable?
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

## splitting the data into training and test data sets
set.seed(144)
split = sample.split(parole$violator , SplitRatio = 0.7)
paroleTrain = subset(parole, split==T)
paroleTest = subset(parole, split==F)

## creating a logistic model
logmod = glm(violator ~ ., data=paroleTrain, family=binomial)
summary(logmod)

## predicting on the test set
predictTest = predict(logmod, newdata=paroleTest , type="response")

## What is the maximum predicted probability of a violation?
max(predictTest)

## threshold of 0.5
table(paroleTest$violator , predictTest > 0.5)
table(paroleTest$violator)

## using ROCR and AUC
ROCRpred = prediction (predictTest , paroleTest$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values) ## 0.7421095

## Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. Answer the following questions based on the model's predictions for this individual. (HINT: You should use the coefficients of your model, the Logistic Response Function, and the Odds equation to solve this problem.)

## According to the model, what are the odds this individual is a violator?
## odds = e((B0 + B1x1 + B2x2 + ... + Bnxn) )
odds = exp(-4.2411574 + (-0.0001756)*50 + (0.3869904)*1 + (0.8867192)*1 +
            (-0.1238867)*3 + (0.0802954)*12 + (1.6119919) * 0 + 0.6837143)
## According to the model, what is the probability this individual is a violator?
## P(y=1) = 1/(1 + e-(B0 + B1x1 + B2x2 + ... + Bnxn) 
prob = 1/(1+exp(-(-4.2411574 + (-0.0001756)*50 + (0.3869904)*1 + (0.8867192)*1 +
                  (-0.1238867)*3 + (0.0802954)*12 + (1.6119919) * 0 + 0.6837143)) )