## Detecting vandalism on wikipedia

rm(list=ls())
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

setwd("D:/Analytics/Analytics_edge/text_analytics/spam")
emails = read.csv("emails.csv", stringsAsFactors=F)
str(emails)
summary(emails)

## how many emails are spam
table(emails$spam) ## 1368

## Which word appears at the beginning of every email in the dataset?
emails[1:2,]

## The nchar() function counts the number of characters in a piece of text. How many characters are in the longest email in the dataset
which.max(nchar(emails$text))
nchar(emails$text[2651])

## shortest email
which.min(nchar(emails$text))
nchar(emails$text[1992])

## document term matrix creation
corpusText = Corpus(VectorSource(emails$text))
corpusText = tm_map(corpusText,tolower)
corpusText = tm_map(corpusText,PlainTextDocument)
corpusText = tm_map(corpusText,removePunctuation)
corpusText = tm_map(corpusText,removeWords,stopwords("english"))
corpusText = tm_map(corpusText,stemDocument)

dtmText = DocumentTermMatrix(corpusText)
sptdm = removeSparseTerms(dtmText,0.95)

emailsSparse = as.data.frame(as.matrix(sptdm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

## most frequent term
sort(colSums(emailsSparse))

## adding the spam variable
emailsSparse$spam = emails$spam

## how many words appear at least 5000 times in ham emails
sort(colSums(emailsSparse[emailsSparse$spam == 0,]))

## how many words appear at least 1000 times in spam emails
sort(colSums(emailsSparse[emailsSparse$spam == 1,]))

## converting to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)

## creating train and test
spl = sample.split(emailsSparse$spam,SplitRatio=0.7)
train = subset(emailsSparse,spl == T)
test = subset(emailsSparse,spl == F)

## creating a log reg mod
logm = glm(spam ~ . , data=train , family="binomial")
predictlogm = predict(logm)
table(train$spam , predictlogm > 0.5)
predROCR = prediction(predictlogm,train$spam)
perfROCR = performance(predROCR,"tpr","fpr")

predictlogm = predict(logm,newdata=test)
table(test$spam , predictlogm > 0.5 )
acc = (1258+376)/nrow(test) ## 0.9511059
predROCR = prediction(predictlogm,test$spam)
perfROCR = performance(predROCR,"tpr","fpr")


plot(perfROCR,colorize = T)
auc = performance(predROCR,"auc")@y.values  ## 1

emailCART = rpart(spam ~ . , data=train , method="class")
predictCART = predict(emailCART)
prp(emailCART)
table(train$spam , predictCART[,2] > 0.5)

predictCART = predict(emailCART,newdata=test)
table(test$spam , predictCART[,2] > 0.5)
acc = (1228 + 386)/nrow(test)

predROCR = prediction(predictCART[,2],test$spam)
perfROCR = performance(predROCR,"tpr","fpr")

auc = performance(predROCR,"auc")@y.values  ## 1

set.seed(123)
spamRF = randomForest(spam~., data=train)
predictRF = predict(spamRF)

predictRF = predict(spamRF , newdata=test)
table(test$spam , predictRF)
acc = (1290+388)/nrow(test)

predROCR = prediction(predictRF,test$spam)
perfROCR = performance(predROCR,"tpr","fpr")

auc = performance(predROCR,"auc")@y.values  ## 1


## How many of the training set predicted probabilities from spamLog are less than 0.00001?
length(which(predictlogm < 0.00001))

## How many of the training set predicted probabilities from spamLog are more than 0.99999?
length(which(predictlogm > 0.00001 & predictlogm < 0.99999))

summary(logm)

## ------------------------------------------

library(slam)

wordCount = rollup(dtmText, 2, FUN=sum)$v
hist(wordCount,breaks=100)
hist(log(wordCount))

emailsSparse$wordcount = log(wordCount)

boxplot(emailsSparse$wordcount~emailsSparse$spam)

set.seed(123)

## creating train and test
spl = sample.split(emailsSparse$spam,SplitRatio=0.7)
train2 = subset(emailsSparse,spl == T)
test2 = subset(emailsSparse,spl == F)

spam2CART = rpart(spam ~ . , data=train2,method="class")
set.seed(123)
spam2RF = randomForest(spam ~ . , data=train2)

predictCART2 = predict(spam2CART,newdata=test2)
table(test2$spam , predictCART2[,2] > 0.5 )
acc = (4173 + 1237)/nrow(test2)

predROCR = prediction(predictCART2[,2],test2$spam)
perfROCR = performance(predROCR,"tpr","fpr")

auc = performance(predROCR,"auc")@y.values  ## 1

predictRF2 = predict(spam2RF,newdata=test2,type="class")
table(test2$spam , predictRF2 )
acc = (1298 + 379)/nrow(test2)

predROCR = prediction(predictRF2,test2$spam)
perfROCR = performance(predROCR,"tpr","fpr")

auc = performance(predROCR,"auc")@y.values  ## 1
