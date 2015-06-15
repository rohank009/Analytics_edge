## analysing enron emails

rm(list=ls())
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

setwd("D:/Analytics/Analytics_edge/text_analytics/enron")
emails = read.csv("energy_bids.csv", stringsAsFactors=F)
str(emails)
summary(tweets)

strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]

## breakdown
table(emails$responsive)

## creating a corpus
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

## converting the corpus to lowercase
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,stopwords("english"))
corpus = tm_map(corpus,stemDocument)

strwrap(corpus[[1]])
dtm = DocumentTermMatrix(corpus)
dtm

## removing sparsity , removing words that do not appear in less than 3% of the documents
dtm = removeSparseTerms(dtm,0.97)
dtm

## creating data frames
labeledterms = as.data.frame(as.matrix(dtm))
labeledterms$responsive = emails$responsive
str(labeledterms)

## splitting the dataset
set.seed(144)
spl = sample.split(labeledterms$responsive,SplitRatio=0.7)
train = subset(labeledterms,spl == T)
test = subset(labeledterms,spl == F)

emailCART = rpart(responsive ~ . , data=train,method = "class")
prp(emailCART)

pred = predict(emailCART,newdata=test)
pred[1:10,]
pred.prob = pred[,2]
table(test$responsive,pred.prob>0.5)
acc = (195 + 25)/nrow(test) ## 0.8560311

table(test$responsive)
baseline_acc = 215/(215+42) ## 0.8365759

predROCR = prediction(pred.prob,test$responsive)
perfROCR = performance(predROCR,"tpr","fpr")

plot(perfROCR,colorize = T)
auc = performance(predROCR,"auc")@y.values  ## 0.7936323

## randomforest
set.seed(144)
colnames(train) = make.names(colnames(train))
colnames(test) = make.names(colnames(test))
emailRF = randomForest(responsive ~ . , data=train)
predRF = predict(emailRF,newdata=test)

table(test$responsive , predRF>0.5)
acc = (203+24)/nrow(test) ##0.8832685
