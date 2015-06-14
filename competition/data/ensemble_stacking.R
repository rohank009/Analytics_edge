#################################
##Programmer: James E. Yonamine
##Date: 9/12/2012
##Basic introduction to stacking
#################################

##load pacakges and data
library(foreign)
library(Zelig)
library(MASS)
library(LiblineaR)
library(foreign)
library(e1071)
library(randomForest)
library(ada)
library("glmnet")
rm(list=ls())
set.seed(2012)

data.train<-read.dta("stacking.train.dta") #make sure to set your working directory
data.test<-read.dta("stacking.test.dta")

y.train<-data.train[,1]
y.train<-as.matrix(y.train)
x.train<-data.train[,-1]
y.test<-data.test[,1]
y.test<-as.matrix(y.test)
x.test<-data.test[,-1]

#####step 1: train models on data.train####
#logits
logit.1 <- glm(y ~ var3 +var4 + var5 +var6 + var7 +var8 + var9 +var10 + var11 +var12 + var13 +var14 +var16 +var17 +var18, family=binomial(link="logit"), data=data.train)
logit.2 <- glm(y ~ var19 + var20 + var21 + var22 + var23 + var24 + var25 + var26 + var27 + var28 +var29 +var30, family=binomial(link="logit"), data=data.train)
logit.3<- glm(y ~var31 +var32 +var33 +var34 + var35 +var36 +var37 +var38 +var39 +var40 + var41 +var42 + var43, family=binomial(link="logit"), data=data.train)

#randomForest -- this is a bit more complicated
sim<-100  
mtry=3

rf.store.train <- matrix(data=0, nrow=nrow(x.train), ncol=sim)
for (k in 1:sim){
  rf.fit <- randomForest(as.factor(y.train)~., data=x.train,  ntree=1, mtry=mtry)
  rf.predict<-predict(rf.fit, newdata=x.train)
  matrix<-as.matrix(rf.predict)
  nrow(matrix)
  rf.store.train[,k]<-as.numeric(matrix[,1])	
}

#LVQ
my.codebook<-lvqinit(x=x.train, cl=y.train, size=10, prior=c(0.5,0.5),k = 2)
my.codebook<-lvq1(x=x.train, cl=y.train, codebk=my.codebook, niter = 100 * nrow(my.codebook$x), alpha = 0.03)

#SVM
svm.fit <- svm(formula = y.train ~ x.train, y=y.train, x=x.train, probability=TRUE)

#####step 2a: build predictions for data.train####
train.predict<- matrix(data=0, nrow=nrow(data.train), ncol=6)#define storage matrix
train.predict[,1]<-predict(logit.1, newdata=data.train, type="response")
train.predict[,2]<-predict(logit.2, newdata=data.train, type="response")
train.predict[,3]<-predict(logit.3, newdata=data.train, type="response")
train.predict[,4]<-as.matrix(rowSums(rf.store.train)/sim)
train.predict[,5]<-knn1(train=my.codebook$x, test=x.train, cl=my.codebook$cl)
train.predict[,6] <- predict(svm.fit, x.train, probability=TRUE)

####step 2b: build predictions for data.test####
test.predict <- matrix(data=0, nrow=nrow(data.test), ncol=6)#define storage matrix
test.predict[,1] <- predict(logit.1, newdata=data.test, type="response")
test.predict[,2] <- predict(logit.2, newdata=data.test, type="response")
test.predict[,3] <- predict(logit.3, newdata=data.test, type="response")

##random forest is a bit more complicated
rf.store.test <- matrix(data=0, nrow=nrow(x.test), ncol=sim)
for (k in 1:sim){
  rf.fit <- randomForest(as.factor(y.train)~., data=x.train,  ntree=1, mtry=mtry)
  rf.predict<-predict(rf.fit, newdata=x.test)
  matrix<-as.matrix(rf.predict)
  nrow(matrix)
  rf.store.test[,k]<-as.numeric(matrix[,1])	
}
test.predict[,4]<-as.matrix(rowSums(rf.store.test)/sim)
test.predict[,5]<-knn1(train=my.codebook$x, test=x.test, cl=my.codebook$cl)
test.predict[,6] <- predict(svm.fit, x.test, probability=TRUE)

####step 3: train SVM on train.predict####
final <- svm(formula = y.test ~ train.predict, y=y.train, x=train.predict, probability=TRUE)

####step 4: use trained SVM to make predictions with test.predict####
final.predict <- predict(final, test.predict, probability=TRUE)
results<-as.matrix(final.predict)
results<-cbind(results, y.test)
fix(results)