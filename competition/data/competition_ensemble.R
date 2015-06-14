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
lbrary(glmnet)
lbrary(adaboost.M1)
setwd("D:/Analytics/Analytics_edge/competition/data")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

## combining the train and test set after removing the popular and unique id variable
NewsTrain1 = subset(NewsTrain,select = -c(UniqueID,Popular))
NewsTest1 = subset(NewsTest,select = -c(UniqueID))

News = rbind(NewsTrain1 , NewsTest1)

## populating missing data
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Opinion")),"OpEd",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Multimedia")),"Science",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Arts")),"Culture",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Business Day")),"Business",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Crosswords/Games")),"Business",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Health")),"Science",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="N.Y. / Region")),"Metro",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Technology")),"Business",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Travel")),"Travel",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="World")),"Foreign",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="U.S.")),"Styles",News$NewsDesk)
# News$NewsDesk = ifelse((News$NewsDesk == "") & (News$SectionName=="Open")),"Open",News$NewsDesk)
##News$NewsDesk = ifelse(News$NewsDesk == "","Missingvalue",News$NewsDesk)
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


# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Business")),"Business Day",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Culture")),"Arts",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Foreign")),"World",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="National")),"U.S.",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="OpEd")),"Opinion",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Science")),"Health",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Sports")),"Sports",News$SectionName)
# News$SectionName = ifelse((nchar(News$SectionName == 0) & (News$NewsDesk=="Styles")),"U.S.",News$SectionName)
##News$SectionName = ifelse(News$SectionName == "",News$NewsDesk,News$SectionName)

News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Business")] = "Business Day"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Culture")] = "Arts"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Foreign")] = "World"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="National")] = "U.S."
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="OpEd")] = "Opinion"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Science")] = "Health"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Sports")] = "Sports"
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="Styles")] = "U.S."
News$SectionName[(News$SectionName == "") & (News$NewsDesk=="TStyle")] = "TStyle"


##table(NewsTrain$SubsectionName[NewsTrain$SectionName== "World"])

# News$SubsectionName = ifelse((nchar(News$SubsectionName == 0) & (News$SectionName=="Business Day")),"Dealbook",News$SubsectionName)
# News$SubsectionName = ifelse((nchar(News$SubsectionName == 0) & (News$SectionName=="World")),"Pacific",News$SubsectionName)
# News$SubsectionName = ifelse((nchar(News$SubsectionName == 0) & (News$SectionName=="U.S.")),"Education",News$SubsectionName)
##News$SubsectionName = ifelse(News$SubsectionName == "",News$SectionName,News$SubsectionName)

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
## converting to R datetime
News$PubDate = strptime(News$PubDate, "%Y-%m-%d %H:%M:%S")

News$NewsDesk = as.factor(News$NewsDesk)
News$SectionName = as.factor(News$SectionName)
News$SubsectionName = as.factor(News$SubsectionName)
News$Weekday = as.factor(News$PubDate$wday)
News$Hour = as.factor(News$PubDate$hour)
News$logwordcount = log(News$WordCount + 1)

## adding date features

News$Questionasked = as.factor(ifelse(is.na(str_locate(News$Headline,"\\?"))[,1],0,1))
News$opinionasked = as.factor(ifelse(News$SectionName == "Opinion",1,0))
News$Questionaskedabs = as.factor(ifelse(is.na(str_locate(News$Abstract,"\\?"))[,1],0,1))
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
sparse = removeSparseTerms(dtm, 0.995)

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
HeadlineWordsTest$dealbook = tail(News$dealbook,nrow(NewsTest))
# levels(HeadlineWordsTest$Questionasked) = c("NO","YES")
# levels(HeadlineWordsTest$Questionaskedabs) = c("NO","YES")
# levels(HeadlineWordsTest$USA) = c("NO","YES")
# levels(HeadlineWordsTest$roomfordebate) = c("NO","YES")
# levels(HeadlineWordsTest$opinionasked) = c("NO","YES")

### CREATING three sets of data

set.seed(9)
HeadlineWordsTrain <- HeadlineWordsTrain[sample(nrow(HeadlineWordsTrain)),]
split <- floor(nrow(HeadlineWordsTrain)/3)
ensembleData <- HeadlineWordsTrain[0:split,]
blenderData <- HeadlineWordsTrain[(split+1):(split*2),]
testingData <- HeadlineWordsTrain[(split*2+1):nrow(HeadlineWordsTrain),]

labelName <- 'Popular'
predictors <- c("logwordcount","Hour","Weekday","Questionasked","Questionaskedabs","NewsDesk","SectionName","SubsectionName","abstractlen","obama","deal","time","busi","can","get","morn","new","rais","read","today","word","daili","small","USA","dealbook","opinionasked","Questionaskedroomfordebate")
folds=5
repeats=1

##myControl <- trainControl(method='cv', number=10, returnResamp='none')
myControl <- trainControl(method='cv', number=folds, repeats=repeats,returnResamp='none', classProbs=TRUE,returnData=FALSE, savePredictions=TRUE,verboseIter=TRUE, allowParallel=TRUE,summaryFunction=twoClassSummary,index=createMultiFolds(ensembleData[,labelName], k=folds, times=repeats))
##  run the data on a gbm model without any enembling to use as a comparative benchmark:

test_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

## function to get the Area Under the Curve (AUC):

preds <- predict(object=test_model, testingData[,predictors])

auc <- roc(testingData[,labelName], as.numeric(preds))
print(auc$auc) 
## plot(varImp(objModel,scale=F))
## We now use 3 models - gbm, glmnet, rf, adaboost as part of our ensembles of models and train them with the ensembleData data set:

model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)
summary(model_gbm)
model_gbm
preds_gbm <- predict(object=model_gbm, testingData[,predictors])
auc <- roc(testingData[,labelName], as.numeric(preds))
print(auc$auc) 
plot(varImp(model_gbm,scale=F))



model_rf <- train(ensembleData[,predictors], ensembleData[,labelName], method='rf', trControl=myControl)
summary(model_rf)
model_rf
preds_rf <- predict(object=model_rf, testingData[,predictors])
auc <- roc(testingData[,labelName], as.numeric(preds))
print(auc$auc) 
plot(varImp(model_rf,scale=F))

model_adab <- train(ensembleData[,predictors], ensembleData[,labelName], method='AdaBoost.M1', trControl=myControl)
summary(model_adab)
model_adab
preds_adab <- predict(object=model_adab, testingData[,predictors])
auc <- roc(testingData[,labelName], as.numeric(preds))
print(auc$auc) 
plot(varImp(model_adab,scale=F))

model_c5 <- train(ensembleData[,predictors], ensembleData[,labelName], method='C5.0', trControl=myControl)
summary(model_c5)
model_c5
preds_c5 <- predict(object=model_c5, testingData[,predictors])
auc <- roc(ifelse(testingData[,labelName] == "YES",1,0), preds)
print(auc$auc) 
plot(varImp(model_c5,scale=F))


## After our 3 models are trained, we use them to predict popular articles on the other two data sets: blenderData and testingData - yes, both!! We need to do this to harvest the predictions from both data sets as we're going to add those predictions as new features to the same data sets. So, as we have 3 models, we're going to add three new columns to both blenderData and testingData:

blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rf, blenderData[,predictors])
blenderData$ada_PROB <- predict(object=model_adab, blenderData[,predictors])
blenderData$c5_PROB <- predict(object=model_c5, blenderData[,predictors])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rf, testingData[,predictors])
testingData$ada_PROB <- predict(object=model_adab, testingData[,predictors])
testingData$c5_PROB <- predict(object=model_c5, testingData[,predictors])
## train a final blending model on the old data and the new predictions (we use gbm but that is completely arbitrary):

predictors <- c("logwordcount","Hour","Weekday","Questionasked","Questionaskedabs","NewsDesk","SectionName","SubsectionName","abstractlen","obama","deal","time","busi","can","get","morn","new","rais","read","today","word","daili","small","USA","dealbook","opinionasked","Questionaskedroomfordebate")
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

summary(final_blender_model)
final_blender_model
preds_c5 <- predict(object=model_c5, testingData[,predictors])
auc <- roc(ifelse(testingData[,labelName] == "YES",1,0), preds)
print(auc$auc) 
plot(varImp(final_blender_model,scale=F))

## we call predict and roc/auc functions to see how our blended ensemble model fared:

preds <- predict(object=final_blender_model$finalModel, testingData[,predictors])
auc <- roc(testingData[,labelName], preds)
print(auc$auc)

table(testingData$Popular,preds)
predensem = predict(final_blender_model,HeadlineWordsTest[,predictors])

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = ifelse(predensem == "YES",1,0))

write.csv(MySubmission, "Submissionrensemble.csv", row.names=FALSE)

##################################################################


#### Logistic Regression prediction
## creating the logistic regression model
## busi + can + get + morn + new + rais + read + today + word + daili + small + Questionasked + opinionasked + logwordcount + NewsDesk + SubsectionName + SectionName + abstractlen + Hour + Weekday
# Newstrainlogm = glm(Popular ~  .  , data=HeadlineWordsTrain , family = "binomial")
# ##Newstrainlogm = step(glm(Popular ~  . , data=HeadlineWordsTrain , family = "binomial"))
# summary(Newstrainlogm)
# Newstrainpredict = predict(Newstrainlogm,method = "response")
# table(NewsTrain$Popular , Newstrainpredict>0.5)
# 
# Newstestpredict = predict(Newstrainlogm , newdata = HeadlineWordsTest, method = "response")
# 
# # Now we can prepare our submission file for Kaggle:
# Newstestpredict = ifelse(Newstestpredict < 0 , 0 , Newstestpredict)
# Newstestpredict = ifelse(Newstestpredict > 1 , 1 , Newstestpredict)
# 
# ######  taking average of logm and RF predictions
# Newstrainpredict = ifelse(Newstrainpredict < 0 , 0 , Newstrainpredict)
# Newstrainpredict = ifelse(Newstrainpredict > 1 , 1 , Newstrainpredict)
# 
# df_trainpredict = as.data.frame(cbind(Newstrainpredict,NewstrainpredictRF))
# df_trainpredict$NewstrainpredictRF = as.numeric(df_trainpredict$NewstrainpredictRF)-1
# df_trainpredict$avgpredict = (0.4 * df_trainpredict$Newstrainpredict + 0.6 * df_trainpredict$NewstrainpredictRF)
# 
# table(NewsTrain$Popular , df_trainpredict$avgpredict>=0.5)
# 
# df_testpredict = as.data.frame(cbind(Newstestpredict,PredictRF))
# df_testpredict$PredictRF = as.numeric(df_testpredict$PredictRF)-1
# df_testpredict$avgpredict = (0.4 * df_testpredict$Newstestpredict + 0.6 * df_testpredict$PredictRF)
# 
# 
# MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = df_testpredict$avgpredict)
# 
# write.csv(MySubmission, "Submissioncombine.csv", row.names=FALSE)
# 

###########################################################
# --------------------------------------------------
set.seed(9)
Yvals = HeadlineWordsTrain$Popular
# "busi","day","deal","ebola","fashion","get", "morn" ,"new" ,"read","rais","Questionasked","Questionaskedabs","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday","opinionasked","USA","roomfordebate"

IndependentVars = HeadlineWordsTrain[c("logwordcount","Hour","Weekday","Questionasked","Questionaskedabs","opinionasked","NewsDesk","SectionName","SubsectionName","abstractlen","obama","deal","time","busi","can","get","morn","new","rais","read","today","word","daili","small")]
IndependentVars$Popular = NULL # Drop the Dependent variable column

fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
trrf = train(IndependentVars, Yvals, method="rf", nodesize = 7 , ntree = 1000,metric="ROC", trControl=fitControl)

predtrainrf = predict(trrf$finalModel,type="prob")
table(HeadlineWordsTrain$Popular , predtrainrf[,2] > 0.5)

predtestrf = predict(trrf$finalModel,HeadlineWordsTest,type="prob")

## ----------------------------------

# set.seed(9)
# Yvals = HeadlineWordsTrain$Popular
# 
# IndependentVars = HeadlineWordsTrain[c("busi", "ebola","deal","obama","polit","york", "rais" ,"today" ,"daili","roomfordebate","Questionasked","opinionasked","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday")]
# 
# IndependentVars$Popular = NULL # Drop the Dependent variable column
# 
# fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)
# 
# trlogm = train(IndependentVars, Yvals, method="glm" , family = "binomial", trControl=fitControl)
# 
# predtrainglm = predict(trlogm$finalModel,type="response")
# table(HeadlineWordsTrain$Popular , predtrainglm > 0.5)
# 
# predtestglm = predict(trlogm$finalModel,HeadlineWordsTest,type="prob")
# 
# 
## ----------------------------------

# set.seed(9)
# Yvals = HeadlineWordsTrain$Popular
# 
# IndependentVars = HeadlineWordsTrain[c("Questionasked","opinionasked","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday")]
# 
# IndependentVars$Popular = NULL # Drop the Dependent variable column
# 
# fitControl <-  trainControl(summaryFunction = twoClassSummary, classProbs = TRUE)
# 
# trgbm = train(IndependentVars, Yvals, method="gbm" , trControl = fitControl,metric = "ROC")
# 
# predtraingbm = predict(trgbm$finalModel,type = "response",n.trees=trgbm$bestTune$n.trees)
# table(HeadlineWordsTrain$Popular , predtraingbm > 0.5)
# 
# predtestgbm = predict(trgbm$finalModel,newdata=HeadlineWordsTest[c("busi", "ebola","deal","obama","polit","york", "rais" ,"today" ,"daili","roomfordebate","Questionasked","opinionasked","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday")],type="response",n.trees=trgbm$bestTune$n.trees)
# 

##  ---------------------------------
# ------------------------
#   modelName = step(glm(     ,    )

## seq(0.00001, 0.01, 0.00001))

###########################################################

# finalpred = (0.1 * predtraingbm + 0.9 * predtrainrf)
# table(HeadlineWordsTrain$Popular , finalpred[,2] > 0.5)
# 
# finalpred = 0.1 * predtestgbm + 0.9 * predtestrf
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predtestrf[,2])

write.csv(MySubmission, "Submissionrfnew.csv", row.names=FALSE)

#########################################
##tapply(NewsTrain$Popular,NewsTrain$SectionName,mean)
############################################

#################### trying ENSEMBLES
###  http://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
set.seed(9)
Yvals = HeadlineWordsTrain$Popular
IndependentVars = HeadlineWordsTrain[c("compani", "editor","group","morn","new","polit", "year" ,"Questionasked","Questionaskedabs","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday")]
IndependentVars1 = HeadlineWordsTest[c("compani", "editor","group","morn","new","polit", "year" ,"Questionasked","Questionaskedabs","logwordcount","NewsDesk","SubsectionName","SectionName","abstractlen","Hour","Weekday")]
my_control <- trainControl(
  method='boot',
  number=25,
  savePredictions=TRUE,
  classProbs=TRUE,
  index=createResample(HeadlineWordsTrain$Popular, 25),
  summaryFunction=twoClassSummary
)

model_list <- caretList(
  Popular ~., data=HeadlineWordsTrain,
  trControl=my_control,
  methodList=c('glm', 'rf')
)

model_list$pred
p <- as.data.frame(predict(model_list, newdata=head(HeadlineWordsTest)))
print(p)

model_list_big <- caretList(
  Yvals~., data=IndependentVars,
  trControl=my_control,
  metric='ROC',
  methodList=c('glm', 'rf'),
  tuneList=list(
    rf1=caretModelSpec(method='rf', tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method='rf', tuneGrid=data.frame(.mtry=10), preProcess='pca'),
    nn=caretModelSpec(method='nnet', tuneLength=2, trace=FALSE)
  )
)

p <- as.data.frame(predict(model_list_big))

p <- as.data.frame(predict(model_list_big, newdata=HeadlineWordsTest))
print(p)


## ROC
xyplot(resamples(model_list))
modelCor(resamples(model_list))

greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

library('caTools')
model_preds <- lapply(model_list, predict, newdata=HeadlineWordsTest, type='prob')
model_preds <- lapply(model_preds, function(x) x[,'M'])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=HeadlineWordsTest)
model_preds$ensemble <- ens_preds
colAUC(model_preds, HeadlineWordsTest$Popular)

varImp(greedy_ensemble)

### CARETSTACK

glm_ensemble <- caretStack(
  model_list, 
  method='glm',
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=HeadlineWordsTest, type='prob')$M
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, HeadlineWordsTest$Popular)

CF/sum(CF)

model_preds2$ensemble <- predict(glm_ensemble, type='prob')$M

library('gbm')
gbm_ensemble <- caretStack(
  model_list, 
  method='gbm',
  verbose=FALSE,
  tuneLength=10,
  metric='ROC',
  trControl=trainControl(
    method='boot',
    number=10,
    savePredictions=TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
model_preds3 <- model_preds
model_preds3$ensemble <- predict(gbm_ensemble, newdata=HeadlineWordsTest, type='prob')$M
colAUC(model_preds3, HeadlineWordsTest$Popular)



# ## american + archiv + art + articl + best + big + billion + 
# book + busi + chang + china + citi + collect + compani + 
#   daili + deal + design + diari + discuss + editor + famili + 
#   fashion + fund + group + herald + hous + includ + like + 
#   make + manag + market + media + million + morn + nation + 
#   new + offer + peopl + photo + polit + rais + report + republican + 
#   say + senat + share + show + state + stori + time + tribun + 
#   unit + year + york + logwordcount + NewsDesk + SectionName + 
#   Weekday + Hour + abstractlen + SubsectionName + Questionasked + 
#   Questionaskedabs


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predtestrf[,2])

write.csv(MySubmission, "Submissionranfor.csv", row.names=FALSE)
