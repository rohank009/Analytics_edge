## Flower

rm(list=ls())

setwd("D:/Analytics/Analytics_edge/clustering/flower")
flower = read.csv("flower.csv", header = FALSE)
str(flower)

flower = as.matrix(flower)
str(flower)

flowervector = as.vector(flower)
str(flowervector)

## computing the distances
distance = dist(flowervector , method = "euclidean")

clusterIntensity = hclust(distance,method = "ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity,k=3, border = "red")
flowerClusters = cutree(clusterIntensity,k=3)
flowerClusters[1:10]

tapply(flowervector , flowerClusters , mean) ## cl1 contains the darkest intensity and cl3 contains the fairest intensity

## creating the image
dim(flowerClusters) = c(50,50)
str(flowerClusters)
image(flowerClusters,axes = FALSE)
image(flower,axes = FALSE , col = grey(seq(0,1, length = 256)))