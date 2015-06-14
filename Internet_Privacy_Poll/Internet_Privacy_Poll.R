## Internet Anonymity poll
setwd("D:/Analytics/Analytics_edge/Internet_Privacy_Poll")

## Reading the csv files
anonymitypoll=read.csv("AnonymityPoll.csv")

## How many people participated in the poll?
summary(anonymitypoll)
str(anonymitypoll)

## How many interviewees responded that they use a smartphone?
## How many interviewees responded that they don't use a smartphone?
## How many interviewees did not respond to the question, 
## resulting in a missing value, or NA, in the summary() output?
table(anonymitypoll$Smartphone)
table(is.na(anonymitypoll$Smartphone))

## Which of the following are states in the Midwest census region? (Select all that apply.)
table(anonymitypoll$State,anonymitypoll$Region=="Midwest")

## Which was the state in the South census region with the largest number of interviewees?
table(anonymitypoll$State,anonymitypoll$Region=="South")

## How many interviewees reported not having used the Internet and not having used a smartphone?
table(anonymitypoll$Internet.Use==0 & anonymitypoll$Smartphone==0)

## How many interviewees reported having used the Internet and having used a smartphone?
table(anonymitypoll$Internet.Use==1 & anonymitypoll$Smartphone==1)

## How many interviewees reported having used the Internet but not having used a smartphone?
table(anonymitypoll$Internet.Use==1 & anonymitypoll$Smartphone==0)

## How many interviewees reported having used a smartphone but not having used the Internet?
table(anonymitypoll$Internet.Use==0 & anonymitypoll$Smartphone==1)

## How many interviewees have a missing value for their Internet use?
table(is.na(anonymitypoll$Internet.Use))

## How many interviewees have a missing value for their smartphone use?
table(is.na(anonymitypoll$Smartphone))

## Use the subset function to obtain a data frame called "limited", 
## which is limited to interviewees who reported Internet use or who reported smartphone use
limited=subset(anonymitypoll,(anonymitypoll$Internet.Use | anonymitypoll$Smartphone) )

## Which variables have missing values in the limited data frame?
summary(limited)

## What is the average number of pieces of personal information on the Internet, 
## according to the Info.On.Internet variable?
mean(limited$Info.On.Internet)

## How many interviewees reported a value of 0 for Info.On.Internet?
## How many interviewees reported the maximum value of 11 for Info.On.Internet?
table(limited$Info.On.Internet)

## What proportion of interviewees who answered the Worry.About.Info question 
## worry about how much information is available about them on the Internet? 
table(limited$Worry.About.Info) ## 386/(404+386)

## What proportion of interviewees who answered the Anonymity.Possible question think 
## it is possible to be completely anonymous on the Internet?
table(limited$Anonymity.Possible) ##278/(278+475)

## What proportion of interviewees who answered the Tried.Masking.Identity question 
## have tried masking their identity on the Internet?
table(limited$Tried.Masking.Identity) ##128/(128+656)

##What proportion of interviewees who answered the Privacy.Laws.Effective question 
## find United States privacy laws effective?
table(limited$Privacy.Laws.Effective) ##186/(186+541)

## Build a histogram of the age of interviewees. 
## What is the best represented age group in the population?
hist(limited$Age)

## What is the largest number of interviewees that have exactly the same value in their Age variable 
## AND the same value in their Info.On.Internet variable? In other words, what is the largest number of overlapping points in the plot plot(limited$Age, limited$Info.On.Internet)?
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

## Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)). 
## What relationship to you observe between Age and Info.On.Internet?
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

## Use the tapply() function to obtain the summary of the Info.On.Internet value, 
## broken down by whether an interviewee is a smartphone user.
tapply(limited$Info.On.Internet,limited$Smartphone,mean)

## What proportion of smartphone users who answered the Tried.Masking.Identity question have 
## tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity=="1",limited$Smartphone,mean,na.rm=T)