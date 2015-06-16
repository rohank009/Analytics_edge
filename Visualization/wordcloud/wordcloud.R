# VISUALIZING TEXT DATA USING WORD CLOUDS
library(tm)
library(wordcloud)
library(RColorBrewer)

setwd("D:/Analytics/Analytics_edge/Visualization/wordcloud")
tweets = read.csv("tweets.csv")
str(tweets)

## creating a corpus
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

## creating a wordcloud

wordcloud(colnames(allTweets) , colSums(allTweets) , scale=c(2,0.25) ,random.order = F )

wordcloud(colnames(allTweets) , colSums(allTweets) , scale=c(2,0.25) ,random.order = F ,rot.per=0.5)

wordcloud(colnames(allTweets) , colSums(allTweets) , scale=c(2,0.25) ,brewer.pal(9, "Blues")[-1:-4])

## replacing the most frequent word form the corpus

corpus = tm_map(corpus, removeWords, c(stopwords("english"),"apple"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(colnames(allTweets) , colSums(allTweets) , scale=c(2,0.25) )

## negative word cloud 

negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(negativeTweets), colSums(negativeTweets),scale=c(2,0.25))