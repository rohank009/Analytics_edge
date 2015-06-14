## Brain

rm(list=ls())
library(flexclust)
setwd("D:/Analytics/Analytics_edge/clustering/brain")
healthy = read.csv("healthy.csv", header = FALSE)

healthymat = as.matrix(healthy)
str(healthymat)

image(healthymat , axes = F , col = grey(seq(0,1,length=256)))
healthyvec = as.vector(healthymat)
distance = dist(healthyvec , method = "euclidean") ## formula wld be n*(n-1)/2
str(healthyvec)

## k-means clustering
set.seed(1)
k=5
kmc = kmeans(healthyvec , centers=k , iter.max = 1000)
str(kmc)
healthyclusters = kmc$cluster

dim(healthyclusters) = c(nrow(healthymat),ncol(healthymat))
image(healthyclusters , axes = F , col = rainbow(k))

tumor = read.csv("tumor.csv", header = FALSE)
tumormat = as.matrix(tumor) 
tumorvec = as.vector(tumormat)

kmc.kcca = as.kcca(kmc , healthyvec)
tumorclusters = predict(kmc.kcca , newdata=tumorvec)
dim(tumorclusters)  = c(nrow(tumormat),ncol(tumormat))
image(tumorclusters , axes=F , col=rainbow(k))