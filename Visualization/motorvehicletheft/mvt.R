## motor vehicle theft

rm(list=ls())
library(ggplot2) 
library(maps)
library(ggmap)
setwd("D:/Analytics/Analytics_edge/Visualization/motorvehicletheft")
mvt = read.csv("mvt.csv", header = TRUE , stringsAsFactors = F)
str(mvt)

## converting date to a readable format
mvt$Date = strptime(mvt$Date , format = "%m/%d/%y %H:%M")

## adding hour and day
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

table(mvt$Weekday)
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

## creating plot
ggplot(WeekdayCounts , aes(x=Var1 , y=Freq)) + geom_line(aes(group=1))
ggplot(WeekdayCounts , aes(x=Var1 , y=Freq)) + geom_line(aes(group=1), linetype=2)
ggplot(WeekdayCounts , aes(x=Var1 , y=Freq)) + geom_line(aes(group=1), alpha=0.3)
## creating var1 as ordered factor
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1,ordered=T , levels = c("Sunday", "Monday","Tuesday","Wednesday","Thursday", "Friday","Saturday"))
ggplot(WeekdayCounts , aes(x=Var1 , y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the week") + ylab ("Total motor vehicle thefts")

## creating a table by weekday and hour
table(mvt$Weekday,mvt$Hour)

dayhourcounts = as.data.frame(table(mvt$Weekday,mvt$Hour))
str(dayhourcounts)
dayhourcounts$Hour = as.numeric(as.character(dayhourcounts$Var2))
ggplot(dayhourcounts , aes(x = Hour , y = Freq)) + geom_line(aes(group=Var1))
ggplot(dayhourcounts , aes(x = Hour , y = Freq)) + geom_line(aes(group=Var1 , color=Var1 , size=2))
dayhourcounts$Var1 = factor(dayhourcounts$Var1,ordered=T , levels = c("Monday","Tuesday","Wednesday","Thursday", "Friday","Saturday","Sunday"))

## creating heat map
ggplot(dayhourcounts , aes(x = Hour , y = Var1)) + geom_tile(aes(fill=Freq))
ggplot(dayhourcounts , aes(x = Hour , y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Theft", low="white" , high="red") + theme(axis.title.y=element_blank())

## getting a map of Chicago
chicago = get_map(location="chicago" , zoom=11)
ggmap(chicago)

pune = get_map(location="pune" , zoom=11)
ggmap(pune)

ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude , y=Latitude))

latloncounts = as.data.frame(table(round(mvt$Longitude,2) , round(mvt$Latitude,2)))
latloncounts$lon = as.numeric(as.character(latloncounts$Var1))
latloncounts$lat = as.numeric(as.character(latloncounts$Var2))
ggmap(chicago) + geom_point(data=latloncounts, aes(x=lon , y=lat , color=Freq , size=Freq))
ggmap(chicago) + geom_point(data=latloncounts, aes(x=lon , y=lat , color=Freq , size=Freq)) + scale_color_gradient(low="yellow" , high="red")
## using geom_tile
ggmap(chicago) + geom_tile(data=latloncounts, aes(x=lon , y=lat , alpha=Freq), fill="red") 
##-----------------------
latloncounts2 = subset(latloncounts , latloncounts$Freq > 0)
ggmap(chicago) + geom_tile(data=latloncounts2, aes(x=lon , y=lat , alpha=Freq), fill="red") 
