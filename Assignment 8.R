# GB 656 Assignment 8
# Xiao Freeman Zhang

rm(list=ls())
# Package required
install.packages("maps")
install.packages("mapproj")
install.packages("NbClust")

#Data Preparation
library(psych)
wine_data <- 
  read.table("~/Desktop/UW - Madison/Fall 2022/GB 656/Moduel 9/winequality-red.csv", sep=";", header=TRUE)
dim(wine_data)
describe(wine_data)[,1:5]

#Visualize
library(corrplot)
rho <- cor(wine_data[,], use="pairwise.complete.obs")
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(rho, tl.cex=.7, order="hclust", col=col3(50))

corrplot(rho, order="hclust", method="shade", col=col3(50), tl.cex=.7)

# 
ss <- function(x)  sum( ( x-mean(x) )^2 )
wss <- NULL
wss[1] <- sum( apply(wine_data,2,ss) )
for (k in 2:10) {
  temp <- kmeans(wine_data, k)
  wss[k] <- sum(temp$withinss)
}

barplot(wss, col="dodgerblue", names.arg=1:length(wss)
        , xlab="Number of Clusters (k)"
        , ylab="Total Within Sum of Squares")
abline(h=0)
title("Within Sum-of-Squares Analysis", col.main="navy")

# Clustering
k <- 5
set.seed(652)
km <- kmeans(wine_data, k)
clust.km <- km$cluster

# Illustrating a cluster
dd <- dist(wine_data, method="euclidean")
hc1 <- hclust(dd, method="average")
hc1 <- hclust(dd, method="complete")
hc1 <- hclust(dd, method="ward.D")
plot(hc1, hang=-1)
rect.hclust(hc1, k=5, border="dodgerblue")
rect.hclust(hc1, k=4, border="blue")
rect.hclust(hc1, k=3, border="red")
rect.hclust(hc1, k=2, border="green")

hc1 <- hclust(dd, method="ward.D")
rect.hclust(hc1, k=5, border="dodgerblue")

# Relabel cluster
clust.hc1 <- cutree(hc1,4)

reord <- function(cluster){
  avg <- tapply(scored$quality, cluster, mean); avg
  ord <- order(avg); ord
  clus <- factor(cluster, levels=ord); table(clus)
  levels(clus) <- 1:length(clus)
  return( as.numeric(as.character(clus)) )
}


scored <- wine_data
scored$clust.km <- reord(clust.km)
scored$clust.hc <- reord(clust.hc1)

tapply(scored$quality, clust.km, mean)
tapply(scored$quality, reord(clust.km), mean)
table(clust.km, reord(clust.km))

tapply(scored$quality, clust.hc1, mean)
tapply(scored$quality, reord(clust.hc1), mean)
table(clust.hc1, reord(clust.hc1))

table(scored$clust.km, scored$clust.hc)




#PCA
pc1 <- prcomp(wine_data)
pc1 <- prcomp(scale(wine_data))
round(pc1$rotation[,1:2], 3)

pcs <- predict(pc1) 
describe(pcs)[,1:5]

dim(pcs)
dim(wine_data)

#PC Correlation metrix
corrplot(cor(pcs)) 

#Prepare scee plot
vars <- apply(pcs, 2, var)
sum(vars); ncol(wine_data); ncol(pcs)


barplot(vars[1:10], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

plot(pc1)

summary(pc1)

# so-called bi-plot
biplot(pc1, col=c("slategrey", "navy"), cex=c(.2, .8))


round(pc1$rotation, 4)[,1:6]

# visualize our clusters in PC space:
col <- c("blue","dodgerblue","lightgreen","pink","red")

par(mfrow=c(1,2))

clust <- scored$clust.km
plot(pcs, type="n", main="k-means")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)


clust <- scored$clust.hc
plot(pcs, type="n", main="hierarchical clustering")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)


# Analyzing the cluster
col <- c("blue", "dodgerblue", "lightgreen", "pink", "red", "maroon", "darkorange")

clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
summ <- apply(wine_data, 2, agg)
t(round(summ,2))

# Include the PCs:
scored2 <- wine_data
scored2$PC1 <- pcs[,1]
scored2$PC2 <- pcs[,2]
scored2$PC3 <- pcs[,3]
clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
SUMMARY <- apply(scored2, 2, agg)
t(round(SUMMARY,2))

#Visualize
names(wine_data)

show <- 1:4
show <- 5:8
show <- 9:12
par(mfrow=c(2,4))
for(i in show){
  boxplot( scored[,i] ~ clust, col=col, varwidth=TRUE)
  abline(h=0, col="navy")
  title(names(scored)[i])
}







