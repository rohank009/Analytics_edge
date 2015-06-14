## Kos

rm(list=ls())
library(flexclust)
library(caret)
setwd("D:/Analytics/Analytics_edge/clustering/airlines")
airlines = read.csv("AirlinesCluster.csv", header = TRUE)

str(airlines)
summary(airlines)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc , airlines)

## running hierarchical clustering algorithm
distance = dist(airlinesNorm , method = "euclidean")

airlinesclust = hclust(distance , method = "ward.D")
plot(airlinesclust)

airlinesclusters = cutree(airlinesclust , k=5)

## how many data points are in cluster - 1
table(airlinesclusters)
airlinehierclusters = split(airlinesNorm , airlinesclusters)

## use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters)
tapply(airlines$Balance , airlinesclusters , mean)
tapply(airlines$QualMiles , airlinesclusters , mean)
tapply(airlines$BonusMiles , airlinesclusters , mean)
tapply(airlines$BonusTrans , airlinesclusters , mean)
tapply(airlines$FlightMiles , airlinesclusters , mean)
tapply(airlines$FlightTrans , airlinesclusters , mean)
tapply(airlines$DaysSinceEnroll , airlinesclusters , mean)

lapply(split(airlinesNorm, airlinesclusters), colMeans)

## creating k-means algorithm
k = 5
set.seed(1000)
kmc = kmeans(airlinesNorm , centers = k)
kHierCluster = split(airlinesNorm , kmc$cluster)

## compare the cluster centroids
kmc$centers