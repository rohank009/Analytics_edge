#VISUALIZING NETWORK DATA
library(ggplot2)
library(igraph)

setwd("D:/Analytics/Analytics_edge/Visualization/network")
edges = read.csv("edges.csv")
str(edges)

users = read.csv("users.csv")
str(users)

## creating a graph
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size=5, vertex.label=NA)
degree(g) >=10

## changing the size for ppl having more friends
V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)

## coloring the graph based on gender
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

## coloring the graph based on school
V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

## coloring the graph based on locale
V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)