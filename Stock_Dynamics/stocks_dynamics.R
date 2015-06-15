## Stock Dynamics
setwd("D:/Analytics/Analytics_edge/Stock_Dynamics")

## Reading the csv files
ibm=read.csv("IBMStock.csv")
boeing=read.csv("BoeingStock.csv")
ge=read.csv("GEStock.csv")
cocacola=read.csv("CocaColaStock.csv")
proctar=read.csv("ProcterGambleStock.csv")

## Converting into date format
str(ibm)
ibm$Date=as.Date(ibm$Date,"%m/%d/%y")
str(ibm)
boeing$Date=as.Date(boeing$Date,"%m/%d/%y")
ge$Date=as.Date(ge$Date,"%m/%d/%y")
cocacola$Date=as.Date(cocacola$Date,"%m/%d/%y")
proctar$Date=as.Date(proctar$Date,"%m/%d/%y")

summary(proctar)
str(proctar)

# earliest year in the dataset
ibm[1,1]
# latest year in the dataset
ibm[480,1]
# mean stock price of IBM
summary(ibm)
# min stock price of GE
summary(ge)
# max stock price of coca cola
summary(cocacola)
# median stock price of Boeing
summary(boeing)
# standard deviation for proctar and gamble
summary(proctar)
sd(proctar$StockPrice)

## visualizing stock dynamics
plot(cocacola$Date,cocacola$StockPrice,type="l",col="red")
lines(proctar$Date, proctar$StockPrice,col="blue", lty=2) ## lty=2 for dashed line
abline(v=as.Date(c("1983-03-01")), lwd=2) ## lwd makes line thicker

## VISUALIZING STOCK DYNAMICS 1995-2005  
colors()
plot(cocacola$Date[301:432], cocacola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(proctar$Date[301:432], proctar$StockPrice[301:432],col="blue", lty=2) ## lty=2 for dashed line
lines(ibm$Date[301:432], ibm$StockPrice[301:432],col="darkgreen", lty=3) ## lty=2 for dotted line
lines(ge$Date[301:432], ge$StockPrice[301:432],col="orange", lty=4) ## lty=2 for alternate dash and dots line
lines(boeing$Date[301:432], boeing$StockPrice[301:432],col="black", lty=5) ## lty=2 for long-dashed line
##  Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date(c("2000-03-01")), lwd=1) ## lwd makes line thicker
## Which stock reaches the highest value in the time period 1995-2005?
abline(v=as.Date(c("1995-01-01")), lwd=1) ## lwd makes line thicker
abline(v=as.Date(c("2005-01-01")), lwd=1) ## lwd makes line thicker
## In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. 
## Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price?
abline(v=as.Date(c("1997-09-01")), lwd=1) ## lwd makes line thicker
abline(v=as.Date(c("1997-11-01")), lwd=1) ## lwd makes line thicker
## In the last two years of this time period (2004 and 2005) 
## which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date(c("2004-01-01")), lwd=1) ## lwd makes line thicker
abline(v=as.Date(c("2005-12-31")), lwd=1) ## lwd makes line thicker
## For IBM, compare the monthly averages to the overall average stock price. 
## In which months has IBM historically had a higher stock price (on average)?
mean(ibm$StockPrice)
tapply(ibm$StockPrice,months(ibm$Date),mean)
## MONTHLY TRENDS
## Repeat the tapply function from the previous problem for each of the other four companies, and use the output to answer the remaining questions.
## General Electric and Coca-Cola both have their highest average stock price in the same month. 
## Which month is this?
mean(ge$StockPrice)
tapply(ge$StockPrice,months(ge$Date),mean)
mean(boeing$StockPrice)
tapply(boeing$StockPrice,months(boeing$Date),mean)
mean(proctar$StockPrice)
tapply(proctar$StockPrice,months(proctar$Date),mean)
mean(cocacola$StockPrice)
tapply(cocacola$StockPrice,months(cocacola$Date),mean)