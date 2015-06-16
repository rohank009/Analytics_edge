## WHO

rm(list=ls())
library(ggplot2) 
setwd("D:/Analytics/Analytics_edge/Visualization/who")
WHO = read.csv("WHO.csv", header = TRUE)
str(WHO)

plot(WHO$GNI ~ WHO$FertilityRate)

## GGPLOT2 requires three objects - data object , aesthetic maping and geometric Object

scatterplot = ggplot(WHO , aes(x = GNI , y = FertilityRate))
scatterplot + geom_point() 

scatterplot + geom_line() 

scatterplot + geom_point() 

## adding color and size and shape
scatterplot + geom_point(color = "blue" , size=3, shape=17) 
scatterplot + geom_point(color = "darkred" , size=3, shape=8) 

## title
scatterplot + geom_point(color = "darkred" , size=3, shape=15) + ggtitle("Fertility Rate Vs Gross National Income")
fertilityGNIplot = scatterplot + geom_point(color = "darkred" , size=3, shape=15) + ggtitle("Fertility Rate Vs Gross National Income")
pdf("myplot.pdf")
print(fertilityGNIplot)
dev.off()

## coloring by region
ggplot(WHO , aes(x=GNI , y = FertilityRate, color=Region)) + geom_point()
## coloring by LifeExpectancy
ggplot(WHO , aes(x=GNI , y = FertilityRate, color=LifeExpectancy)) + geom_point()
## changing the plot to find correlation
ggplot(WHO , aes(x=FertilityRate , y = Under15)) + geom_point()
## changing the plot to find correlation
ggplot(WHO , aes(x=log(FertilityRate) , y = Under15)) + geom_point()
## lin reg mod
model = lm(Under15 ~ log(FertilityRate) , data=WHO)
summary(model)
## adding the model
ggplot(WHO , aes(x=log(FertilityRate) , y = Under15)) + geom_point() + stat_smooth(method="lm")
## changing the confidence interval
ggplot(WHO , aes(x=log(FertilityRate) , y = Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99)
ggplot(WHO , aes(x=log(FertilityRate) , y = Under15)) + geom_point() + stat_smooth(method="lm", se=F , color="orange")

## color schemes
ggplot(WHO, aes(x = FertilityRate, y = Under15 , color=Region)) + geom_point() 
## for color blind
ggplot(WHO, aes(x = FertilityRate, y = Under15 , color=Region)) + geom_point() + scale_color_brewer(palette="Dark2") 
