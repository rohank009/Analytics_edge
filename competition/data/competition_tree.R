## KAGGLE COMPETITION using logm
rm(list=ls())
library(stringr)
library(tm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(pROC)
setwd("D:/Analytics/Analytics_edge/competition/data")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

## combining the train and test set after removing the popular and unique id variable
NewsTrain1 = subset(NewsTrain,select = -c(UniqueID,Popular))
NewsTest1 = subset(NewsTest,select = -c(UniqueID))

News = rbind(NewsTrain1 , NewsTest1)

## converting to R datetime
News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")

News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)
News$Weekday = as.factor(News$PubDate$wday)
News$Hour = as.factor(News$PubDate$hour)
News$logwordcount = log(News$WordCount + 1)

## adding date features

News$Weekday = News$PubDate$wday
News$hour = News$PubDate$hour
# 
# NewsTrain$hour = NewsTrain$PubDate$hour
# NewsTest$hour = NewsTest$PubDate$hour

## creating corpus

CorpusHeadline = Corpus(VectorSource(c(News$Headline)))

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

HeadlineWordsTrain$Popular = as.factor(NewsTrain$Popular)
HeadlineWordsTrain$logwordcount = head(News$logwordcount,nrow(NewsTrain))
HeadlineWordsTrain$NewsDesk = head(News$NewsDesk,nrow(NewsTrain))
HeadlineWordsTrain$SectionName = head(News$SectionName,nrow(NewsTrain))
HeadlineWordsTrain$Weekday = head(News$Weekday,nrow(NewsTrain))
HeadlineWordsTrain$Hour = head(News$Hour,nrow(NewsTrain))
HeadlineWordsTrain$abstractlen = head(str_length(News$Abstract),nrow(NewsTrain))
HeadlineWordsTrain$SubsectionName = head(News$SubsectionName,nrow(NewsTrain))

HeadlineWordsTest$logwordcount = tail(News$logwordcount,nrow(NewsTest))
HeadlineWordsTest$NewsDesk = tail(News$NewsDesk,nrow(NewsTest))
HeadlineWordsTest$SectionName = tail(News$SectionName,nrow(NewsTest))
HeadlineWordsTest$Weekday = tail(News$Weekday,nrow(NewsTest))
HeadlineWordsTest$Hour = tail(News$Hour,nrow(NewsTest))
HeadlineWordsTest$abstractlen = tail(str_length(News$Abstract),nrow(NewsTest))
HeadlineWordsTest$SubsectionName = tail(News$SubsectionName,nrow(NewsTest))


##+ Headline + NewsDesk + abstractbin + SectionName + Weekday + hour
###### CART model
Newstraintree = rpart(Popular ~ . , data = HeadlineWordsTrain, method="class", minbucket=25)
prp(Newstraintree)

# Make predictions
Newstrainpredict = predict(Newstraintree, type = "class")
table(HeadlineWordsTrain$Popular , Newstrainpredict)

PredictCART = predict(Newstraintree, newdata = HeadlineWordsTest, type = "class")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredictCART)

write.csv(MySubmission, "SubmissionCART.csv", row.names=FALSE)

####### RandomForest model
set.seed(9)
NewstrainRF = randomForest(Popular ~ ., data = HeadlineWordsTrain,type = "prob", ntree=1000, nodesize=7 )

# Make predictions
NewstrainpredictRF = predict(NewstrainRF, type = "class")
table(HeadlineWordsTrain$Popular , NewstrainpredictRF)

PredictRF = predict(NewstrainRF, newdata = HeadlineWordsTest, type = "class")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredictRF)

write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

################# TREE using TRAIN

nf <- trainControl(method="cv", number=10)

cg <- expand.grid(.cp=seq(0.001,0.002, 0.0002))

train(Popular ~., data= HeadlineWordsTrain, tuneGrid=cg, method="rpart", trControl=nf) ## take the min cp value from output and use in next step

fitcp <- rpart(Popular ~., data= HeadlineWordsTrain, method="class", cp=0.0018)

predcp <- predict(fitcp)
table(HeadlineWordsTrain$Popular , predcp[,2]>0.5)

PredictCART = predict(fitcp, newdata = HeadlineWordsTest, type = "class")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredictCART)

write.csv(MySubmission, "SubmissionTREE.csv", row.names=FALSE)

###################################### RANDOM FOREST
# 
fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
# 
# Note AUC is called "ROC"
# 
tr = train(Popular~., HeadlineWordsTrain, method="rf", nodesize=5, ntree=500, metric="ROC", trControl=fitControl)
Newstrainpredict = predict(tr, type = "class")
table(HeadlineWordsTrain$Popular , Newstrainpredict)

PredictRF = predict(Newstraintree, newdata = HeadlineWordsTest, type = "class")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredictRF)

write.csv(MySubmission, "SubmissionRF.csv", row.names=FALSE)

#####################################################################

#### Logistic Regression prediction
## creating the logistic regression model
## logwordcount + NewsDesk + SubsectionName + SectionName + abstractlen + Hour + Weekday  
Newstrainlogm = glm(Popular ~ ., data=HeadlineWordsTrain , family = "binomial")
summary(Newstrainlogm)
Newstrainpredict = predict(Newstrainlogm,method = "response")
table(NewsTrain$Popular , Newstrainpredict>0.5)

Newstestpredict = predict(Newstrainlogm , newdata = HeadlineWordsTest, method = "response")

# Now we can prepare our submission file for Kaggle:
Newstestpredict = ifelse(Newstestpredict < 0 , 0 , Newstestpredict)
Newstestpredict = ifelse(Newstestpredict > 1 , 1 , Newstestpredict)

######  taking average of logm and RF predictions
Newstrainpredict = ifelse(Newstrainpredict < 0 , 0 , Newstrainpredict)
Newstrainpredict = ifelse(Newstrainpredict > 1 , 1 , Newstrainpredict)

df_trainpredict = as.data.frame(cbind(Newstrainpredict,NewstrainpredictRF))
df_trainpredict$NewstrainpredictRF = as.numeric(df_trainpredict$NewstrainpredictRF)-1
df_trainpredict$avgpredict = (0.4*df_trainpredict$Newstrainpredict) + (0.6*df_trainpredict$NewstrainpredictRF)

table(NewsTrain$Popular , df_trainpredict$avgpredict>=0.5)

df_testpredict = as.data.frame(cbind(Newstestpredict,PredictRF))
df_testpredict$PredictRF = as.numeric(df_testpredict$PredictRF)-1
df_testpredict$avgpredict = (0.4*df_testpredict$Newstestpredict) + (0.6*df_testpredict$PredictRF)



MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = df_testpredict$avgpredict)

write.csv(MySubmission, "Submissioncombine.csv", row.names=FALSE)