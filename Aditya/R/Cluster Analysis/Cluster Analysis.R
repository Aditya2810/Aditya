setwd("D:/R files")
dungaree <- read.csv("dungaree_updated.csv")
dungaree <- dungaree[, -1]
dungaree <- dungaree[, -5]
dungaree.norm <- sapply(dungaree, scale)
install.packages("NbClust")
library(NbClust)
set.seed(42)
devAskNewPage(ask=TRUE)

#Running with max of 10 clusters
nc <- NbClust(dungaree.norm, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Perform k-means cluster analysis
fit.km <- kmeans(dungaree.norm, 3, nstart=25)
fit.km$size

# calcualte cluster centroidsfit.km$centers
fit.km$centers
library(cluster)
clusplot(pam(dungaree.norm,3))

#wss plot to explain variance 
wssplot <- function(dungaree.norm, nc=10, seed=42) 
{wss <- (nrow(dungaree.norm)-1)*sum(apply(dungaree.norm,2,var)) 
for (i in 2:nc) {set.seed(42) 
  wss[i] <- sum(kmeans(dungaree.norm, centers=i)$withinss)} 
plot(1:nc, wss, type="b", xlab="Number of clusters", ylab="within groups sum of squares")}
wssplot(dungaree.norm, nc=10, seed=42)

#Running with max of 6 clusters
nc1 <- NbClust(dungaree.norm, min.nc=2, max.nc=6, method="kmeans")
table(nc1$Best.n[1,])
barplot(table(nc1$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Perform k-means cluster analysis
fit.km1 <- kmeans(dungaree.norm, 5, nstart=25)
fit.km1$size

# calcualte cluster centroidsfit.km$centers
fit.km1$centers
library(cluster)
clusplot(pam(dungaree.norm,5))

################################################################################

#Exercise 2- Heirarchical clustering

setwd("D:/R files")
pharma1 <- read.csv("Pharmaceuticals.csv")
pharma1 <- pharma1[, -c(1:2)]
pharma1 <- pharma1[, -c(10:12)]
pharma1.norm <- sapply(pharma1, scale)
library(NbClust)
devAskNewPage(ask=TRUE)
nc2 <- NbClust(pharma1.norm, distance="euclidean", min.nc=2, max.nc=4, method="average")
table(nc2$Best.n[1,])
barplot(table(nc2$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")
d <- dist(pharma1.norm)
fit.average <- hclust(d, method="average")
plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering")
clusters <- cutree(fit.average, k=3)
table(clusters)
aggregate(pharma1.norm, by=list(cluster=clusters), median)
rect.hclust(fit.average, k=3)
library(cluster)
clusplot(pam(pharma1.norm,3))

#k-means for pharmaceutical dataset
library(NbClust)
set.seed(42)
devAskNewPage(ask=TRUE)
nc3 <- NbClust(pharma1.norm, min.nc=2, max.nc=4, method="kmeans")
table(nc3$Best.n[1,])
barplot(table(nc3$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

# Perform k-means cluster analysis
fit.km2 <- kmeans(pharma1.norm, 2, nstart=20)
fit.km2$size

# calcualte cluster centroidsfit.km$centers
fit.km2$centers
library(cluster)
clusplot(pam(pharma1.norm,2))