## http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
## http://www.jstatsoft.org/v28/i05/paper
## http://amunategui.github.io/blending-models/
## KAGGLE COMPETITION using logm
rm(list=ls())
library(stringr)
library(tm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(pROC)
library(gbm)
setwd("D:/Analytics/Analytics_edge/competition/data")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

## combining the train and test set after removing the popular and unique id variable
NewsTrain1 = subset(NewsTrain,select = -c(UniqueID,Popular))
NewsTest1 = subset(NewsTest,select = -c(UniqueID))

News = rbind(NewsTrain1 , NewsTest1)

## populating missing data
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Opinion")] = "OpEd"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Multimedia")] = "Science"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Arts")] = "Culture"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Business Day")] = "Business"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Crosswords/Games")] = "Business"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Health")] = "Science"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="N.Y. / Region")] = "Metro"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Technology")] = "Business"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Travel")] = "Travel"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="World")] = "Foreign"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="U.S.")] = "Styles"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="Open")] = "Open"
News$NewsDesk[(News$NewsDesk == "") & (News$SectionName=="") & (News$SubsectionName=="")] = "missing"

News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Business")] = "Business Day"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Culture")] = "Arts"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Foreign")] = "World"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="National")] = "U.S."
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="OpEd")] = "Opinion"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Science")] = "Health"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Sports")] = "Sports"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Styles")] = "U.S."
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="TStyle")] = "TStyle"
News$SectionName[(News$SectionName=="") & (News$SubsectionName=="")] = News$NewsDesk[(News$SectionName=="") & (News$SubsectionName=="")]

News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Business Day")] = "Dealbook"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="World")] = "Asia Pacific"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="U.S.")] = "Education"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Arts")] = "Arts"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Crosswords/Games")] = "Crosswords/Games"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Health")] = "Health"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Magazine")] = "Magazine"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="N.Y. / Region")] = "N.Y. / Region"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Open")] = "Open"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Opinion")] = "Room For Debate"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Sports")] = "Sports"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Technology")] = "Technology"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="Travel")] = "Travel"
News$SubsectionName[(News$SubsectionName == "") & (News$SectionName=="TStyle")] = "TStyle"
News$SubsectionName[(News$SubsectionName == "")] = (paste(News$NewsDesk[(News$SubsectionName == "")],News$SectionName[(News$SubsectionName == "")] ,sep=""))

## converting to R datetime
News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")

News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)
News$Weekday = as.factor(News$PubDate$wday)
News$Hour = as.factor(News$PubDate$hour)
News$logwordcount = log(News$WordCount + 1)
News$puzzle =grepl("puzzle",News$Abstract,ignore.case=T)

## adding date features
News$opinionasked = as.factor(ifelse(News$SectionName == "Opinion",1,0))
News$Questionasked = as.factor(ifelse(is.na(str_locate(News$Headline,"\\?"))[,1],0,1))
News$Questionaskedabs = as.factor(ifelse(is.na(str_locate(News$Abstract,"\\?"))[,1],0,1))
News$Questionaskedroomfordebate = as.factor(ifelse ( (is.na(str_locate(News$Abstract,"\\?"))[,1]) & (News$SubsectionName == "Room For Debate") & (News$SectionName == "Opinion") ,0,1))
News$puzzle = as.factor(News$puzzle)
News$USA = as.factor(ifelse(News$SectionName == "U.S.",1,0))
News$roomfordebate = as.factor(ifelse(News$SubsectionName == "Room For Debate",1,0))
News$dealbook = as.factor(ifelse(News$SubsectionName == "Dealbook",1,0))

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

##HeadlineWordsTest = tail(HeadlineWords[c("morn","new","polit","rais","read")], nrow(NewsTest))
#####################################################

HeadlineWordsTrain$Popular = as.factor(NewsTrain$Popular)
levels(HeadlineWordsTrain$Popular) = c("NO","YES")
HeadlineWordsTrain$logwordcount = head(News$logwordcount,nrow(NewsTrain))
HeadlineWordsTrain$NewsDesk = head(News$NewsDesk,nrow(NewsTrain))
HeadlineWordsTrain$SectionName = head(News$SectionName,nrow(NewsTrain))
HeadlineWordsTrain$Weekday = head(News$Weekday,nrow(NewsTrain))
HeadlineWordsTrain$Hour = head(News$Hour,nrow(NewsTrain))
HeadlineWordsTrain$abstractlen = head(str_length(News$Abstract),nrow(NewsTrain))
HeadlineWordsTrain$SubsectionName = head(News$SubsectionName,nrow(NewsTrain))
HeadlineWordsTrain$Questionasked = head(News$Questionasked,nrow(NewsTrain))
HeadlineWordsTrain$opinionasked = head(News$opinionasked,nrow(NewsTrain))
HeadlineWordsTrain$Questionaskedabs = head(News$Questionaskedabs,nrow(NewsTrain))
HeadlineWordsTrain$Questionaskedroomfordebate = head(News$Questionaskedroomfordebate,nrow(NewsTrain))
HeadlineWordsTrain$puzzle = head(News$puzzle,nrow(NewsTrain))
HeadlineWordsTrain$USA = head(News$USA,nrow(NewsTrain))
HeadlineWordsTrain$roomfordebate = head(News$roomfordebate,nrow(NewsTrain))
HeadlineWordsTrain$dealbook = head(News$dealbook,nrow(NewsTrain))

# levels(HeadlineWordsTrain$Questionasked) = c("NO","YES")
# levels(HeadlineWordsTrain$Questionaskedabs) = c("NO","YES")
# levels(HeadlineWordsTrain$opinionasked) = c("NO","YES")

HeadlineWordsTest$logwordcount = tail(News$logwordcount,nrow(NewsTest))
HeadlineWordsTest$NewsDesk = tail(News$NewsDesk,nrow(NewsTest))
HeadlineWordsTest$SectionName = tail(News$SectionName,nrow(NewsTest))
HeadlineWordsTest$Weekday = tail(News$Weekday,nrow(NewsTest))
HeadlineWordsTest$Hour = tail(News$Hour,nrow(NewsTest))
HeadlineWordsTest$abstractlen = tail(str_length(News$Abstract),nrow(NewsTest))
HeadlineWordsTest$SubsectionName = tail(News$SubsectionName,nrow(NewsTest))
HeadlineWordsTest$Questionasked = tail(News$Questionasked,nrow(NewsTest))
HeadlineWordsTest$opinionasked = tail(News$opinionasked,nrow(NewsTest))
HeadlineWordsTest$Questionaskedabs = tail(News$Questionaskedabs,nrow(NewsTest))
HeadlineWordsTest$USA = tail(News$USA,nrow(NewsTest))
HeadlineWordsTest$roomfordebate = tail(News$roomfordebate,nrow(NewsTest))
HeadlineWordsTest$Questionaskedroomfordebate = tail(News$Questionaskedroomfordebate,nrow(NewsTest))
HeadlineWordsTest$section = tail(News$section,nrow(NewsTest))
HeadlineWordsTest$puzzle = tail(News$puzzle,nrow(NewsTest))
HeadlineWordsTest$USA = tail(News$USA,nrow(NewsTest))
HeadlineWordsTest$roomfordebate = tail(News$roomfordebate,nrow(NewsTest))
HeadlineWordsTest$dealbook = tail(News$dealbook,nrow(NewsTest))
# levels(HeadlineWordsTest$Questionasked) = c("NO","YES")
# levels(HeadlineWordsTest$Questionaskedabs) = c("NO","YES")
# levels(HeadlineWordsTest$USA) = c("NO","YES")
# levels(HeadlineWordsTest$roomfordebate) = c("NO","YES")
# levels(HeadlineWordsTest$opinionasked) = c("NO","YES")


# 
###########################################################
# --------------------------------------------------
set.seed(9)
Yvals = HeadlineWordsTrain$Popular
# "busi","day","deal","ebola","fashion","get", "morn" ,"new" ,"read","rais","Questionasked","Questionaskedabs","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday","opinionasked","USA","roomfordebate"

IndependentVars = HeadlineWordsTrain[c("logwordcount","Hour","Weekday","Questionaskedabs","NewsDesk","SectionName","SubsectionName","abstractlen","obama","deal","time","busi","can","get","morn","new","rais","read","today","word","daili","small","USA","dealbook","opinionasked","puzzle","Questionaskedroomfordebate")]
predictors = c("logwordcount","Hour","Weekday","Questionaskedabs","NewsDesk","SectionName","SubsectionName","abstractlen","obama","deal","time","busi","can","get","morn","new","rais","read","today","word","daili","small","USA","dealbook","opinionasked","puzzle","Questionaskedroomfordebate")
IndependentVars$Popular = NULL # Drop the Dependent variable column

fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
model_rf = train(IndependentVars, Yvals, method="rf", nodesize = 7 , ntree = 1000,metric="ROC", trControl=fitControl)
predtrainrf = predict(model_rf$finalModel,type="prob")
table(HeadlineWordsTrain$Popular , predtrainrf[,2] > 0.5)

summary(model_rf)
model_rf

predtestrf = predict(model_rf,HeadlineWordsTest,type="prob")

### modelLookup(model = "rf")

################### WHY do we use TRAIN ?#######################################################################

## with the help of train() and pre-specified let's say 5-fold cross-validation, for every mtry of interest, you will get 5 AUCs, mean and sd. So train(), instead of point estimate, gives you a value with sd, that provides you with a feeling of how well your AUC may (or may not) generalize.

## So, in sum, the main difference between randomForest() and train: the former gives a single model, the latter, searches through grid and spits at you a model, which it thinks will generalize best.

########################## training GBM  ###########################################
set.seed(9)

model_gbm <- train(IndependentVars, Yvals , method='gbm', trControl=fitControl)
summary(model_gbm)
model_gbm

predtraingbm = predict(model_gbm,type="prob")
table(HeadlineWordsTrain$Popular , predtraingbm[,2] > 0.5)

predtestgbm = predict(model_gbm,HeadlineWordsTest[,predictors],type="prob")

## finding ROC
roc1 = roc(HeadlineWordsTrain$Popular , predtraingbm[,2])
roc2 = roc(HeadlineWordsTrain$Popular , predtrainrf[,2])

###########################################


final_pred_train = as.data.frame(cbind (unlist(predtrainrf[,2]) , unlist(predtraingbm[,2]) ))
colnames(final_pred_train) = c("rf","gbm")

final_pred_train$pred = (0.5 * final_pred_train$rf) + (0.5 * final_pred_train$gbm)
table(HeadlineWordsTrain$Popular , final_pred_train$pred > 0.5)

final_pred_test = as.data.frame(cbind (unlist(predtestrf[,2]) , unlist(predtestgbm[,2]) ))
colnames(final_pred_test) = c("rf","gbm")

final_pred_test$pred = (0.75 * final_pred_test$rf) + (0.25 * final_pred_test$gbm)


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = final_pred_test$pred)
write.csv(MySubmission, "Submissionensem.csv", row.names=FALSE)

###########################################