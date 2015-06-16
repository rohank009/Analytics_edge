## Household

rm(list=ls())
library(ggplot2) 
library(reshape2)
setwd("D:/Analytics/Analytics_edge/Visualization/household")
household = read.csv("households.csv", header = TRUE)
str(household)

## ggplot requires data in form of year , group fraction
household[,1:2]

## melting the data frame
head(melt(household , id="Year"))

household[,1:3]

melt(household , id="Year")[1:10,]
ggplot(melt(household , id="Year"), aes(x=Year , y = value , color=variable)) + geom_line(size=2) + geom_point(size=5) + ylab("percentage of Households")
