## CART for claimsdata
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
setwd("D:/Analytics/Analytics_edge/trees/claims")
claims = read.csv("ClaimsData.csv")
str(claims)
summary(claims)

table(claims$bucket2009)/nrow(claims)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimstrain = subset(claims,spl == T)
claimstest = subset(claims,spl == F)
str(claimstrain)

## average age of patients
avgage = mean(claimstrain$age)

## What proportion of people in the training set (ClaimsTrain) had at least one diagnosis code for diabetes?
table(claim(strain$diabetes)
104672/nrow(claimstrain)

## baseline method wherein the 2009 cost are same as 2008 cost
table(claimstest$bucket2009, claimstest$bucket2008)
accuracy = (110138 + 10721 + 2774 + 1539 + 104 )/nrow(claimstest) ## =0.6838135

## creating penalty matrix
penaltymatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=T,nrow=5)

## multiply the classification matrix by penalty matrix
as.matrix(table(claimstest$bucket2009, claimstest$bucket2008)) * penaltymatrix
penaltyerror = sum(as.matrix(table(claimstest$bucket2009, claimstest$bucket2008)) * penaltymatrix)/nrow(claimstest)

## Suppose that instead of the baseline method discussed in the previous video, we used the baseline method of predicting the most frequent outcome for all observations. This new baseline method would predict cost bucket 1 for everyone.
## What would the accuracy of this baseline method be on the test set?

table(claimstest$bucket2009 == 1, claimstest$bucket2009)
## accuracy=0.67127
penaltyerror = (0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(claimstest)

## cp value ha already been calculated hence as the data set is too big we won't calculate the cp value again
claimstree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=claimstrain , method="class" , cp=0.00005)
## plotting the tree
prp(claimstree)

## making predcitions
predicttest = predict(claimstree , newdata=claimstest , type="class")
## classification matrix
table(claimstest$bucket2009 , predicttest)
accuracy = (114141 + 16102 + 118 + 201)/nrow(claimstest)
penaltyerror = sum(as.matrix(table(claimstest$bucket2009 , predicttest)) * penaltymatrix)/nrow(claimstest)

## fixing the penaty error
claimstree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=claimstrain , method="class" , cp=0.00005,parms=list(loss=penaltymatrix))
## making predcitions
predicttest = predict(claimstree , newdata=claimstest , type="class")
## classification matrix
table(claimstest$bucket2009 , predicttest)
accuracy = (94310 + 18942 + 4692 + 636 + 2)/nrow(claimstest) ## 0.6472746
penaltyerror = sum(as.matrix(table(claimstest$bucket2009 , predicttest)) * penaltymatrix)/nrow(claimstest) ## 0.6418161
