## movie lens

rm(list=ls())

setwd("D:/Analytics/Analytics_edge/clustering/movielens")
movies = read.table("movielens.txt", header=FALSE, sep="|", quote = "\"")
str(movies)

colnames(movies) = c("ID","Title","ReleaseDate","VideaReleaseDate", "IMDB", "Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideaReleaseDate=NULL
movies$IMDB=NULL

movies = unique(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)

distances = dist(movies[2:20] , method = "euclidean")
clusterMovies = hclust(distances,method = "ward.D2")

## plotting the dendrogram
plot(clusterMovies)

## assigning the cluster names
clusterGroups = cutree(clusterMovies,k=10)

tapply(movies$Action , clusterGroups , mean)
tapply(movies$Romance , clusterGroups , mean)

## you can create a spreadsheet of 10 clusters and label them action or adventure or comedy etc
subset(movies , Title == "Men in Black (1997)" )
clusterGroups[257]

Cluster2 = subset(movies , clusterGroups == 2)

## creating only 2 clusters and finding out which genre the movies belong to in cluster 2
clusterGroups1 = cutree(clusterMovies,k=2)
Cluster1 = subset(movies , clusterGroups1 == 2)

Cluster1$title[1:10]
movies[movies$Title == "Mighty Aphrodite (1995)",]
movies[movies$Title == "Clerks (1994)",]

tapply(movies$Comedy , clusterGroups1 , mean)