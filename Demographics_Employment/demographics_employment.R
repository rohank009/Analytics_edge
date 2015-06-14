## Demographics and Employment
setwd("D:/Analytics/Analytics_edge/Demographics_Employment")

## Reading the csv files
cps=read.csv("CPSData.csv")
metro=read.csv("MetroAreaCodes.csv")
country=read.csv("CountryCodes.csv")

## View the dataset
## How many interviewees are in the dataset?
summary(cps)
str(cps)

## Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? 
## Please enter the name exactly how you see it.
table(cps$Industry)

## Which state has the fewest interviewees?
sort(table(cps$State))
min(table(cps$State))
max(table(cps$State))

## What proportion of interviewees are citizens of the United States?
table(cps$Citizenship)
sum(cps$Citizenship!="Non-Citizen")/length(cps$Citizenship)

## For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? 
table(cps$Hispanic,cps$Race)

## Which variables have at least one interviewee with a missing (NA) value?
summary(cps)

## Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset. 
## We will try to determine if there is a pattern in the missing values of the Married variable. 
## The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for whether the Married variable is missing. 
## We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). 
## Which is the most accurate
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

## As mentioned in the variable descriptions, MetroAreaCode is missing if an interviewee does not live in a metropolitan area. 
## Using the same technique as in the previous question, answer the following questions about people who live in non-metropolitan areas.

## How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? 
## For this question, treat the District of Columbia as a state (even though it is not technically a state).

## How many states had all interviewees living in a metropolitan area? Again, 
## treat the District of Columbia as a state.
table(cps$State, is.na(cps$MetroAreaCode))

## Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(cps$Region, is.na(cps$MetroAreaCode))

## Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
## Which state has the largest proportion of non-metropolitan interviewees, 
## ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(cps$MetroAreaCode),cps$State,mean))

## How many observations (codes for metropolitan areas) are there in MetroAreaMap?
summary(metro)
str(metro)
summary(country)
str(country)

## Review the new version of the CPS data frame with the summary() and str() functions. 
## What is the name of the variable that was added to the data frame by the merge() operation?
cps = merge(cps, metro, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(cps)
str(cps)

## Which of the following metropolitan areas has the largest number of interviewees?
sort(table(cps$MetroArea))

## Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? Hint: Use tapply() with mean, as in the previous subproblem. 
## Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(cps$Hispanic==1,cps$MetroArea,mean))

## Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, 
## determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(cps$Race=="Asian",cps$MetroArea,mean))

## determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean,na.rm=T))

## merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result
cps = merge(cps, country, by.x="CountryOfBirthCode", by.y="Code",all.x=T)
summary(cps)

## Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(cps$Country))

## What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area 
## have a country of birth that is not the United States? For this computation, don't include people from this metropolitan area who have a missing country of birth
table(cps$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", cps$Country != "United States")

## Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? 
## Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question
sort(table(cps$MetroArea[which(cps$Country=="India")]))
sort(table(cps$MetroArea[which(cps$Country=="Somalia")]))