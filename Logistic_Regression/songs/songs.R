## songs
##library("caTools")
##library("ROCR")
library("mice")
setwd("D:/Analytics/Analytics_edge/Logistic_Regression/songs")
songs = read.csv("songs.csv")
str(songs)
summary(songs)

table(songs$year == 2010)

## How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(songs$artistname =="Michael Jackson")

## Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
subset(songs,artistname =="Michael Jackson" & Top10 == 1 )

## The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). What are the values of this variable that occur in our dataset? Select all that apply.
unique(songs$timesignature)
table(songs$timesignature)

## Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
songs[which.max(songs$tempo),]

## We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split the data into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases.

## How many observations (songs) are in the training set?
songsTrain = subset(songs,year <= 2009)
songsTest = subset(songs,year == 2010)
str(songsTrain)

## building the logistic regression
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[, !(names(songsTrain) %in% nonvars)]
songsTest = songsTest[, !(names(songsTest) %in% nonvars)]

songslog = glm(Top10 ~ ., data=songsTrain, family=binomial)
summary(songslog)

## What is the correlation between the variables "loudness" and "energy" in the training set?
cor(songsTrain[c("loudness","energy")] )

## Create Model 2, which is Model 1 without the independent variable "loudness".
songslog2 = glm(Top10 ~ .-loudness, data=songsTrain, family=binomial)
summary(songslog2)

## Now, create Model 3, which should be exactly like Model 1, but without the variable "energy".
songslog3 = glm(Top10 ~ .-energy, data=songsTrain, family=binomial)
summary(songslog3)

## make predictions
predictTest = predict(songslog3 , newdata = songsTest , type = "response")
table(songsTest$Top10 , predictTest > 0.45)

## How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set), using a threshold of 0.45?

## What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(19 + 40) ## 0.3220339
## What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309 + 5) ## 0.9840764