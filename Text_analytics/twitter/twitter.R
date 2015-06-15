## Logistic Regression and classification trees
rm(list=ls())
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
setwd("D:/Analytics/Analytics_edge/text_analytics/twitter")
tweets = read.csv("tweets.csv", stringsAsFactors=F)
str(tweets)
summary(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
tweets$Positive = as.factor(tweets$Avg >= -1)

corpus = Corpus(VectorSource(tweets$Tweet))

## converting to lower
corpus = tm_map(corpus,tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


## remove punctuation
corpus = tm_map(corpus,removePunctuation)

## remove stopwords
corpus = tm_map(corpus,removeWords,c("apple",stopwords("english")))

## stem document
corpus = tm_map(corpus,stemDocument)

## computing the frequencies for words
frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])

##find words apperaing more times in the document
findFreqTerms(frequencies,lowfreq=20)

## remove the words that appear lesser times
sparse = removeSparseTerms(frequencies,0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$negative = tweets$Negative

set.seed(123)
split = sample.split(tweetsSparse$negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse , split == T)
testSparse = subset(tweetsSparse , split == F)

##words appearing at least 100 times
findFreqTerms(frequencies,lowfreq=100)

tweetCART = rpart(negative ~ . , data=trainSparse , method = "class")
prp(tweetCART)

predictCART = predict(tweetCART , newdata=testSparse , type = "class")
table(testSparse$negative , predictCART)
accuracy = (294+18)/nrow(testSparse)

table(testSparse$negative)
acc = 300/355 = 0.8450704

## using randomforest
set.seed(123)
tweetRF = randomForest(negative ~ . , data=trainSparse)
predictRF = predict(tweetRF , newdata=testSparse)
table(testSparse$negative , predictRF)
acc = (291+25)/nrow(testSparse) = 0.8901408

## building a log reg mod
logm = glm(negative ~ . , data=trainSparse,family="binomial")
predictlogm = predict(logm , newdata=testSparse,type="response")
table(testSparse$negative , predictlogm > 0.5)
acc = (245+34)/nrow(testSparse)