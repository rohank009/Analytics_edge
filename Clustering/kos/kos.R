## Kos

rm(list=ls())
library(flexclust)
setwd("D:/Analytics/Analytics_edge/clustering/kos")
kos = read.csv("dailykos.csv", header = TRUE)

distance = dist(kos, method = "euclidean")
kosclust = hclust(distance , method = "ward.D")
plot(kosclust)
rect.hclust(kosclust,k=7, border = "red")
kosclusters = cutree(kosclust,k=7)

cluster1 = subset(kos , kosclusters == 1)
cluster2 = subset(kos , kosclusters == 2)
cluster3 = subset(kos , kosclusters == 3)
cluster4 = subset(kos , kosclusters == 4)
cluster5 = subset(kos , kosclusters == 5)
cluster6 = subset(kos , kosclusters == 6)
cluster7 = subset(kos , kosclusters == 7)

## cluster having max elements
length(cluster1)
length(cluster2)
length(cluster3)
length(cluster4)
length(cluster5)
length(cluster6)
length(cluster7)

## Alternate way
HierCluster = split(kos, kosclusters)
nrow(HierCluster[[1]])

## This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently
tail(sort(colMeans(cluster1)))

## for cluster 2 , 3 , 4 , 5 , 6 , 7
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

## running K-means
set.seed(1000)
k=7
kmc = kmeans(kos , centers=k)
str(kmc)
kHierCluster = split(kos , kmc$cluster)

## OUTPUT THE 6 TOP WORDS IN EACH CLUSTER 
tail(sort(colMeans(kHierCluster[[3]]))) ## corresponds to iraq war
tail(sort(colMeans(kHierCluster[[2]]))) ## corresponds to democratic party leaders

## Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering
table(kosclusters, kmc$cluster) ## 116 (80.6%) of the observations in K-Means Cluster 2 also fall in Hierarchical Cluster 7

## Which Hierarchical Cluster best corresponds to K-Means Cluster 3
table(kosclusters, kmc$cluster)