## KAGGLE COMPETITION using text analytics
rm(list=ls())
library(stringr)
library(tm)

setwd("D:/Analytics/Analytics_edge/competition/data")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

## converting to R datetime
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

## adding new features
NewsTrain$wordcountrange = round(NewsTrain$WordCount,-2)
NewsTrain$wordcountrange = ifelse(NewsTrain$wordcountrange > 5000 , 10000 , NewsTrain$wordcountrange)
NewsTrain$wordcountrange = ifelse(NewsTrain$wordcountrange > 3000 & NewsTrain$wordcountrange < 5001 , 5000 , NewsTrain$wordcountrange)

NewsTrain$abstractbin = as.numeric(cut(str_length(NewsTrain$Abstract),10))
NewsTrain$snippetbin = as.numeric(cut(str_length(NewsTrain$Snippet),10))

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)


NewsTest$wordcountrange = round(NewsTest$WordCount,-2)
NewsTest$wordcountrange = ifelse(NewsTest$wordcountrange > 5000 , 10000 , NewsTest$wordcountrange)
NewsTest$wordcountrange = ifelse(NewsTest$wordcountrange > 3000 & NewsTest$wordcountrange < 5001 , 5000 , NewsTest$wordcountrange)

NewsTest$abstractbin = as.numeric(cut(str_length(NewsTest$Abstract),10))
NewsTest$snippetbin = as.numeric(cut(str_length(NewsTest$Snippet),10))

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)


## adding date features

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

## ---------Applying text analytics

## creating corpus

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

## creating a term matrix
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$WordCount = log(NewsTrain$WordCount + 1)
HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk
HeadlineWordsTrain$Abstract = NewsTrain$Abstract
HeadlineWordsTrain$SectionName = NewsTrain$SectionName
HeadlineWordsTrain$Weekday = NewsTrain$Weekday

HeadlineWordsTest$WordCount = log(NewsTest$WordCount)
HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk
HeadlineWordsTest$Abstract = NewsTest$Abstract
HeadlineWordsTest$SectionName = NewsTest$SectionName
HeadlineWordsTest$Weekday = NewsTest$Weekday

## creating the logistic model
Headlinestrainlogm = glm(Popular ~ . , data=HeadlineWordsTrain , family = "binomial")
Headlinestrainpredict = predict(Headlinestrainlogm,type = "response")
table(HeadlineWordsTrain$Popular , Headlinestrainpredict>0.5)

Newstestpredict = predict(Headlinestrainlogm , newdata = HeadlineWordsTest, method = "response")

# Now we can prepare our submission file for Kaggle:
Newstestpredict = ifelse(Newstestpredict < 0 , 0 , Newstestpredict)
Newstestpredict = ifelse(Newstestpredict > 1 , 1 , Newstestpredict)
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = Newstestpredict)

write.csv(MySubmission, "Submissiontextrangecount.csv", row.names=FALSE)
