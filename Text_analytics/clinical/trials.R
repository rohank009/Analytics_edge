## Detecting vandalism on wikipedia

rm(list=ls())
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

setwd("D:/Analytics/Analytics_edge/text_analytics/clinical")
trials = read.csv("clinical_trial.csv", stringsAsFactors=F)
str(trials)
summary(trials)

## How many characters are there in the longest abstract?
which.max(nchar(trials$abstract))
nchar(trials$abstract[664])

## How many search results provided no abstract?
nrow(trials[(nchar(trials$abstract) == 0),])

## Find the observation with the minimum number of characters in the title (the variable "title") out of all of the observations in this dataset. What is the text of the title of this article? Include capitalization and punctuation in your response, but don't include the quotes.
which.min(nchar(trials$title))
trials$title[1258]

## creating a document term matrix for title
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle,tolower)
corpusTitle = tm_map(corpusTitle,PlainTextDocument)
corpusTitle = tm_map(corpusTitle,removePunctuation)
corpusTitle = tm_map(corpusTitle,removeWords,stopwords("english"))
corpusTitle = tm_map(corpusTitle,stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
findFreqTerms(dtmTitle,lowfreq=80)
sparseTitle = removeSparseTerms(dtmTitle,0.95)
dtmTitle = as.data.frame(as.matrix(sparseTitle))


## creating a document term matrix for abstract
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract,tolower)
corpusAbstract = tm_map(corpusAbstract,PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract,removePunctuation)
corpusAbstract = tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusAbstract = tm_map(corpusAbstract,stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
findFreqTerms(dtmAbstract,lowfreq=80)
sparseAbstract = removeSparseTerms(dtmAbstract,0.95)
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

colnames(dtmAbstract) = make.names(colnames(dtmAbstract))
colnames(dtmTitle) = make.names(colnames(dtmTitle))

## What is the most frequent word stem across all the abstracts? 
sort(colSums(dtmAbstract))

## creating models
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

## combining data frame
dtm = cbind(dtmTitle,dtmAbstract)
dtm$trial = trials$trial

## creating CART
set.seed(144)
spl = sample.split(dtm$trial , SplitRatio=0.7)
train = subset(dtm,spl == T)
test = subset(dtm,spl == F)

## accuracy of the baseline model on the training set
table(train$trial)
acc = (730)/nrow(train) ## 0.5606759

trialCART = rpart(trial ~ . , data=train , method="class")
prp(trialCART)

## predictions
predCART= predict(trialCART) # require probablities hence type variable not require

## What is the maximum predicted probability for any result
max(predCART[2])

table(train$trial , predCART[,2] > 0.5)
acc = (631 + 441)/nrow(train) ## 0.8233487
sensitivity = 441/(441+131) ## 0.770979
specificity = 631/(631 + 99) ## 0.8643836

predCART = predict(trialCART,newdata=test,type="class")
table(test$trial , predCART)
acc = (261+162)/nrow(test) ##0.7580645

predCART = predict(trialCART,newdata=test)
pred.prob = predCART[,2]

## ROCR
predROCR = prediction(pred.prob,test$trial)
perfROCR = performance(predROCR,"tpr","fpr")

plot(perfROCR,colorize = T)
auc = performance(predROCR,"auc")@y.values  ## 0.7936323

