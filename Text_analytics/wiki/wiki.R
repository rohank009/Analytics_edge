## Detecting vandalism on wikipedia

rm(list=ls())
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

setwd("D:/Analytics/Analytics_edge/text_analytics/wiki")
wiki = read.csv("wiki.csv", stringsAsFactors=F)
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)
## How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

## Create the corpus for the Added column, and call it "corpusAdded".
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
## Remove the English-language stopwords.
corpusAdded = tm_map(corpusAdded,removeWords , stopwords("english"))
## Stem the words.
corpusAdded = tm_map(corpusAdded,stemDocument)
## Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

## finding words based on frequency 
findFreqTerms(dtmAdded,lowfreq=20)

## removing sparsity
sparseAdded = removeSparseTerms(dtmAdded,0.997)

## creating a data frame and adding "A" at the beginning of the column name
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

## following the above steps for "removed" column


## Create the corpus for the Added column, and call it "corpusAdded".
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
## Remove the English-language stopwords.
corpusRemoved = tm_map(corpusRemoved,removeWords , stopwords("english"))
## Stem the words.
corpusRemoved = tm_map(corpusRemoved,stemDocument)
## Build the DocumentTermMatrix, and call it dtmAdded.
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

## finding words based on frequency 
findFreqTerms(dtmRemoved,lowfreq=20)

## removing sparsity
sparseRemoved = removeSparseTerms(dtmRemoved,0.997)

## creating a data frame and adding "A" at the beginning of the column name
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(sparseRemoved))
str(wordsRemoved)

## combining the 2 data frames
wikiWords = cbind(wordsAdded , wordsRemoved)
## adding the vandal column
wikiWords$Vandal = wiki$Vandal

set.seed(123)
spl = sample.split(wikiWords$Vandal , SplitRatio = 0.7)
trainwiki = subset(wikiWords, spl == T)
testwiki = subset(wikiWords, spl == F)

## What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(testwiki$Vandal)
baseline_acc = 618/nrow(testwiki) ## 0.5313844

## CART
wikiCART = rpart(Vandal ~ . , data=trainwiki , method = "class")
prp(wikiCART)
predwiki = predict(wikiCART,newdata = testwiki , type="class")

table(testwiki$Vandal , predwiki)
acc = (618 + 13)/nrow(testwiki) ## 0.5425623

## searching for website addresses to find vandal
wikiWords2 = wikiWords
##Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART = rpart(Vandal ~ . , data=wikiTrain2 , method = "class")
prp(wikiCART)
predwiki = predict(wikiCART,newdata = wikiTest2 , type="class")

table(wikiTest2$Vandal , predwiki)
acc = (602 + 65)/nrow(wikiTest2) ## 0.5735168

## Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words themselves
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
avgAdded = mean(wikiWords2$NumWordsAdded) ## 4.050052

## creating new sets and creating CART
wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART = rpart(Vandal ~ . , data=wikiTrain2 , method = "class")
prp(wikiCART)
predwiki = predict(wikiCART,newdata = wikiTest2 , type="class")

table(wikiTest2$Vandal , predwiki)
acc = (513 + 256)/nrow(wikiTest2) ## 0.661221

## creating new dataset
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

## building CART
wikiTrain4 = subset(wikiWords3, spl==TRUE)

wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCART = rpart(Vandal ~ . , data=wikiTrain4 , method = "class")
prp(wikiCART)
predwiki = predict(wikiCART , newdata = wikiTest4 , type="class")

table(wikiTest4$Vandal , predwiki)
acc = (567 + 291)/nrow(wikiTest4) ## 0.7377472
