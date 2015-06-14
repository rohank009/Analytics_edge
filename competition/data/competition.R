## KAGGLE COMPETITION
library(stringr)
library(tm)
rm(list=ls())
setwd("D:/Analytics/Analytics_edge/competition/data")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(NewsTrain)
table(NewsTrain$Popular)
table(NewsTrain$NewsDesk , NewsTrain$Popular) ## OpEd 408/(408+113)
table(NewsTrain$SectionName , NewsTrain$Popular) ## Opinion 425/(425+182)
table(NewsTrain$SubsectionName , NewsTrain$Popular) ##

summary(NewsTrain)


## converting to R datetime
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

table(str_length(NewsTrain$Abstract) , NewsTrain$Popular) ## till 220 more popular
table(str_length(NewsTrain$Snippet) , NewsTrain$Popular) ## till 200 more popular
table(NewsTrain$WordCount , NewsTrain$Popular) ## till 200 more popular

NewsTrain$wordcountrange = round(NewsTrain$WordCount,-2)
NewsTrain$wordcountrange = ifelse(NewsTrain$wordcountrange > 5000 , 10000 , NewsTrain$wordcountrange)
NewsTrain$wordcountrange = ifelse(NewsTrain$wordcountrange > 3000 & NewsTrain$wordcountrange < 5001 , 5000 , NewsTrain$wordcountrange)

NewsTrain$abstractbin = as.numeric(cut(str_length(NewsTrain$Abstract),10))
NewsTrain$snippetbin = as.numeric(cut(str_length(NewsTrain$Snippet),10))
NewsTrain$logwordcount = log(NewsTrain$WordCount + 1)

table(sort(NewsTrain$wordcountrange) , NewsTrain$Popular)
table(NewsTrain$abstractbin , NewsTrain$Popular)
table(NewsTrain$snippetbin , NewsTrain$Popular)
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTrain$hour = NewsTrain$PubDate$hour

NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SectionName = as.factor(NewsTrain$SectionName)

## applying above computations on test data set
NewsTest$wordcountrange = round(NewsTest$WordCount,-2)
NewsTest$wordcountrange = ifelse(NewsTest$wordcountrange > 5000 , 10000 , NewsTest$wordcountrange)
NewsTest$wordcountrange = ifelse(NewsTest$wordcountrange > 3000 & NewsTest$wordcountrange < 5001 , 5000 , NewsTest$wordcountrange)

NewsTest$abstractbin = as.numeric(cut(str_length(NewsTest$Abstract),10))
NewsTest$snippetbin = as.numeric(cut(str_length(NewsTest$Snippet),10))
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTest$hour = NewsTest$PubDate$hour
NewsTest$logwordcount = log(NewsTest$WordCount + 1)

NewsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
NewsTest$SectionName = as.factor(NewsTest$SectionName)

## variales to consider for the model - NewsDesk SectionName abstractbin snippetbin wordcountrange 

## creating the logistic regression model
Newstrainlogm = glm(Popular ~ NewsDesk + SectionName + Weekday + hour + logwordcount , data=NewsTrain , family = "binomial")
Newstrainpredict = predict(Newstrainlogm,method = "response")
table(NewsTrain$Popular , Newstrainpredict>0.5)

Newstestpredict = predict(Newstrainlogm , newdata = NewsTest, method = "response")

# Now we can prepare our submission file for Kaggle:
Newstestpredict = ifelse(Newstestpredict < 0 , 0 , Newstestpredict)
Newstestpredict = ifelse(Newstestpredict > 1 , 1 , Newstestpredict)
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = Newstestpredict)

write.csv(MySubmission, "Submissionrangecount.csv", row.names=FALSE)


