#lab9: clustering, k-means, pam & clara, hierarchical clustering
#author: Andrzej Janusz
# translated and extended by Marcin Szczuka

#1. K-means.
#2. Visualisation of clustering.
#3. Cluster quality measures.
#4. PAM algorithm and its modification - clara.
#5.Hierarchical agglomerative clustering.
#6. Hierarchical divisive clustering.

install.packages("cluster")
install.packages("proxy")

library(cluster)
library(proxy)

#Basic K-means works like this:
#1. Choose by random  k points in attribute-value space as initial group (cluster) centres.
#2. Assign each objects to the neares centre hence creating k groups (clusters).
#3. Select new k centres by calculating means for objects in each of groups created in 2.
#4. Repeat 2 and 3 until convergence criterion is satisfied.

data(agriculture)
?agriculture
fix(agriculture)
rownames(agriculture)

#first, we take look at data
plot(agriculture, xlab = "GNP", ylab = "Agriculture in GNP (percentage)",
     main = "Agriculture in GNP for the old UE cuntries")
text(x = agriculture[,1]+.5, y = agriculture[,2], rownames(agriculture))

#Przyklad implementacji kmeans "krok po kroku" dla danych agriculture (wersja Lloyd-a)
#1. Losowy wybor poczatkowych srodkow grup:
centerList = list(runif(2, min(agriculture[,1]), max(agriculture[,1])),
                  runif(2, min(agriculture[,2]), max(agriculture[,2])))
names(centerList) = c("x", "y")

centerList

plot(agriculture, xlab = "GNP", ylab = "Agriculture in GNP (percentage)",
     main = "Agriculture in GNP for the old UE cuntries")
text(x = agriculture[,1] + 0.5, y = agriculture[,2], rownames(agriculture))
points(x = centerList$x, y = centerList$y, col = c("Red", "Green"))
text(x = centerList$x + 0.5, y = centerList$y, c("Gr1", "Gr2"), col = c("Red", "Green"))

#2. Przydzielanie obiektow do srodkow grup:
distanceTo1 = dist(agriculture, y = matrix(c(centerList$x[1], centerList$y[1]), nrow=1))
distanceTo2 = dist(agriculture, y = matrix(c(centerList$x[2], centerList$y[2]), nrow=1))

clusteringVec = vector()
for(i in 1:nrow(agriculture)) {
    if(distanceTo1[i] < distanceTo2[i]) clusteringVec[i] = 1
    else clusteringVec[i] = 2
}
names(clusteringVec) = rownames(agriculture)

clusteringVec

plot(agriculture, xlab = "GNP", ylab = "Agriculture in GNP (percentage)",
     main = "Agriculture in GNP for the old UE cuntries", col = c("Red", "Green")[clusteringVec])
text(x = agriculture[,1] + 0.5, y = agriculture[,2], rownames(agriculture),
     col = c("Red", "Green")[clusteringVec])
points(x = centerList$x, y = centerList$y, col = c("Red", "Green"))
text(x = centerList$x + 0.5, y = centerList$y, c("Gr1", "Gr2"), col = c("Red", "Green"))

#3. Obliczanie nowych srodkow grup:
clusterSizes = table(clusteringVec)
tmpMeans = rowsum(agriculture, clusteringVec)
tmpMeans = t(as.matrix(tmpMeans)) %*% diag(1/clusterSizes)
centerList$x = tmpMeans[1,]
centerList$y = tmpMeans[2,]
rm(tmpMeans)

plot(agriculture, xlab = "GNP", ylab = "Agriculture in GNP (percentage)",
     main = "Agriculture in GNP for the old UE cuntries", col = c("Red", "Green")[clusteringVec])
text(x = agriculture[,1] + 0.5, y = agriculture[,2], rownames(agriculture),
     col = c("Red", "Green")[clusteringVec])
points(x = centerList$x, y = centerList$y, col = c("Red", "Green"))
text(x = centerList$x + 0.5, y = centerList$y, c("Gr1", "Gr2"), col = c("Red", "Green"))

#4. Powtarzamy punkty 2 i 3 do osiagniecia zbieznosci.


#The whole algorithm:
myKMeans = function(dataSet, k = 5, method = "Euclidean") {

  centerList = list()
  dataSet = scale(dataSet,T,T)
  for(i in 1:ncol(dataSet))  centerList[[i]] = runif(k, min(dataSet[,i]), max(dataSet[,i]))
  endFlag = F
  oldClusteringVec = sample(1:k, nrow(dataSet), T)
  while(!endFlag) {
      distMatrix = apply(as.data.frame(centerList), 1,
                         function(x, dataS) return(dist(dataS, y = matrix(x, nrow=1), method = method)),
                         dataSet)

      clusteringVec = apply(distMatrix, 1, which.min)

      clusterSizes = table(clusteringVec)
      tmpMeans = rowsum(dataSet, clusteringVec)
      tmpMeans = t(as.matrix(tmpMeans)) %*% diag(1/clusterSizes)
      centerList = t(tmpMeans)
      rm(tmpMeans)
      if(all(clusteringVec == oldClusteringVec)) endFlag = T
      oldClusteringVec = clusteringVec
  }
  return(clusteringVec)
}

clusteringVec = myKMeans(agriculture, k = 2, method = "Manhattan")
plot(agriculture, xlab = "GNP", ylab = "Agriculture in GNP (percentage)",
     main = "Agriculture in GNP for the old UE cuntries", col = c("Red", "Green")[clusteringVec])
text(x = agriculture[,1] + 0.5, y = agriculture[,2], rownames(agriculture),
     col = c("Red", "Green")[clusteringVec])


#Let's check another data set
data(pluton)
?pluton
fix(pluton)
pluton = as.data.frame(scale(pluton,T,T))

dim(pluton)
plot(pluton)

clustering1 = myKMeans(pluton, k = 2, method = "Euclidean")
plot(pluton, col = c("Red", "Green")[clustering1])
X11()

#Obviously, R has a readily avaliable, standard implementation of k-means
clustering2 = kmeans(pluton, centers = 5, iter.max = 50, nstart = 1, algorithm = "Lloyd")
plot(pluton, col = (1:5 + 1)[clustering2$cluster])
X11()

clustering3 = kmeans(pluton, centers = 5, iter.max = 50, nstart = 150, algorithm = "Lloyd")
plot(pluton, col = (1:5 + 1)[clustering3$cluster])
X11()


#Which clustering is the best?
#How to visualise clustering for data with many dimensions?

#We use Principal Component Analysis (PCA) to project data on few most important dimensions.


#PCA can now make it possible to plot data
clustering1 = kmeans(pluton, centers = 2, iter.max = 50, nstart = 1, algorithm = "Lloyd")

pca = prcomp(pluton)
par(mfrow =c(1,3))
plot(pca$x, col = clustering1$cluster + 1, main="Clustering1")
newCenters = clustering1$centers %*% pca$rotation
points(x = newCenters[,1], y = newCenters[,2], col = 1:2 + 1)
text(x = newCenters[,1] + 0.2, y = newCenters[,2],
     paste("Gr", 1:2, sep=""), col = 1:2 + 1)
plot(pca$x, col = clustering2$cluster + 1, main="Clustering2")
newCenters = clustering2$centers %*% pca$rotation
points(x = newCenters[,1], y = newCenters[,2], col = 1:5 + 1)
text(x = newCenters[,1] + 0.2, y = newCenters[,2],
     paste("Gr", 1:5, sep=""), col = 1:5 + 1)
plot(pca$x, col = clustering3$cluster + 1, main="Clustering3")
newCenters = clustering3$centers %*% pca$rotation
points(x = newCenters[,1], y = newCenters[,2], col = 1:5 + 1)
text(x = newCenters[,1] + 0.2, y = newCenters[,2],
     paste("Gr", 1:5, sep=""), col = 1:5 + 1)
rm(newCenters, pca)
X11()

#What if visualisation is not enough?
#We need some measure to compare and rank clusterings.
#For example: average of squares of distance from the group centre within each group.



withinSumOfSquares = vector()
for(i in 1:5) withinSumOfSquares[i] = sum((dist(pluton[clustering2$clust == i,],
                                                matrix(clustering2$center[i,],nrow=1)))^2)

#This measure is used in standard K-means, its main advantage is simplicity
clustering1$withinss
mean(clustering1$withinss)

clustering2$withinss
mean(clustering2$withinss)

meanSS = c(mean(clustering1$withinss), mean(clustering2$withinss))
cat("Clustering no. ", which.min(meanSS), " is better than ", which.max(meanSS), "\n", sep="")
rm(clustering1, clustering2, clustering3, meanSS)

#Anothger issue: how to select the best number of groups?
pomVec = rep(0,20)
for(i in 2:20)  {
   pomVec[i] = mean(kmeans(pluton, centers = i, iter.max = 20, nstart = 150)$withinss)
}
plot(pomVec, type = "b")

#Is the number 20 of clusters the optimal one? (NO!)
clustering = kmeans(pluton, centers = 20, iter.max = 20, nstart = 150)
pca = prcomp(pluton)
plot(pca$x, col = clustering$cluster + 1, main="Podzial na 20 grup")
newCenters = clustering$centers %*% pca$rotation
points(x = newCenters[,1], y = newCenters[,2], col = 1:20 + 1)
text(x = newCenters[,1] + 0.2, y = newCenters[,2],
     paste("Gr", 1:20, sep=""), col = 1:20 + 1)
     
#We need another measure, e.g, silhouette coefficient.
#For single object i the slhouette coefficient is given by:
#s(i) = ( b(i) - a(i) ) / max( a(i), b(i) )
#where a(i) is the average distance (dissimilarity) from the objects belonging to the same claster as i,
#and b(i) is the minimum of average distances to objects from different clusters.
#Silhouette width of a cluster ( and whole clustering)  is an average of silhouette coefficient for all objects (in cluster). 



silhouetteVec = rep(0,20)
for(i in 2:20)  {
  clustering = kmeans(pluton, centers = i, iter.max = 20, nstart = 150)
  silhouetteVec[i] = mean(silhouette(clustering$clust, dist(pluton))[,3])
  cat(i, silhouetteVec[i], "\n", sep="\t")
}
cat("According to silhouette coefficient the  division into  ", which.max(silhouetteVec), " groups is optimal \n", sep="")

clustering = kmeans(pluton, centers = which.max(silhouetteVec), iter.max = 20, nstart = 150)
plot(pca$x[,1:2], col = clustering$cluster + 1, main="Podzial na 3 grupy")
newCenters = clustering$centers %*% pca$rotation
points(x = newCenters[,1], y = newCenters[,2], col = 1:which.max(silhouetteVec) + 1)
text(x = newCenters[,1] + 0.2, y = newCenters[,2],
     paste("Gr", 1:which.max(silhouetteVec), sep=""), col = 1:which.max(silhouetteVec) + 1)

#Clustering may also be visualised using clusplot:
clusterNames = paste("Gr", 1:which.max(silhouetteVec), sep="")
clusplot(pluton, clusterNames[clustering$cluster], labels = 2, stand = T, cor = T, shade = T)

#It is also possible to use so called silhouette plot:
plot(silhouette(clustering$clust, dist(pluton)), do.n.k = TRUE, do.clus.stat = T,
     main=paste("Silhouette for clustering into  ", which.max(silhouetteVec), " groups", sep=""),
     xlab = "", ylab = "Objects in groups (sorted w.r.t. silhouette width)", line=1)
mtext(expression("Silhouette width"* s[i]), side = 1, line = 3)
X11()

# PAM clustering - Partitioning Around Medoids - k-medoids
pamClustering1 = pam(pluton, k = 3, metric = "manhattan")
plot(pamClustering1, labels = 2, stand = T, cor = T, shade = T)

#Clustering using distance (dissimilarity) matrix
distMatrix = dist(pluton, method = "Manhattan")
pamClustering2 = pam(distMatrix, k = 3)
plot(pamClustering2, labels = 2, stand = T, cor = T, shade = T, data = pluton)
X11()

#For this data there is no difference between PAM and k-means. PAM is less confused by outliers.
par(mfrow =c(1,2))
plot(prcomp(pluton)$x)
pluton = rbind(pluton, c(7, -1, 7, 1))
pca = prcomp(pluton)
plot(pca$x)

kmeansClustering = kmeans(pluton, centers = 3, iter.max = 20, nstart = 150)
plot(pca$x, col = kmeansClustering$cluster + 1, main="kmeans - clustering into 3 groups")

pamClustering = pam(pluton, k = 3, metric = "manhattan")
plot(pca$x, col = pamClustering$clustering + 1, main="PAM - clustering into 3 groups")

# PAM is quite computationally demanding.
# In R a heuristic, "approximate", version of PAM is implemented and named  clara
?clara

claraClustering = clara(pluton, k = 3, metric = "manhattan", samples = 5, sampsize = 10, pamLike = T)
plot(pca$x, col = claraClustering$clustering + 1, main="clara - clustering into 3 groups")
X11()

#Let's try on larger data:
install.packages("mlbench")
library(mlbench)

dataSet = mlbench.2dnormals(10000,3,r=3)$x
plot(dataSet)

par(mfrow =c(1,2))

system.time({
  pamClustering = pam(dataSet, k = 3, metric = "euclidean");
  plot(dataSet, col = pamClustering$clustering + 1, main="PAM - clustering into 3 groups");
})

system.time({
  claraClustering = clara(dataSet, k = 3, metric = "euclidean", samples = 100, sampsize = 50, pamLike = T);
  plot(dataSet, col = claraClustering$clustering + 1, main="clara - clustering into 3 groups");
})

#Hierarchical clustering

#Two approaches: agglomerative and divisive.

#Agglomerative (bottom-up) clustering starts with as many clusters as there are objects, i.e., each object becomes cluster. 
#Then nearest clusters are aggregated using so called "linkage function". 
#Typical such linkage functions: "single", "average" and "complete".
#Implementation in  R: agnes, hclust
?agnes

data(pluton)
pluton = as.data.frame(scale(pluton,T,T))

distM = dist(pluton, method = "euclidean")
agnesClustSingle = agnes(distM, method = "single")
plot(agnesClustSingle)
X11()

agnesClustAve = agnes(distM, method = "average")
plot(agnesClustAve, which.plots = 2)
X11()

agnesClustComplete = agnes(distM, method = "complete")
plot(agnesClustComplete, which.plots = 2)
X11()

print(agnesClustSingle)
plot(pca$x, col = cutree(agnesClustSingle, k = 3) + 1, main="agnes - clustering into 3 groups")

#The other approach (divisive, top-down)starts by assigning all objects to one cluster
#Then, it divides the clusters until each object becomes a cluster.
#Implementation in R: diana
?diana   #heuristic based on dividing always the group with the largest "diameter"

dianaClustering = diana(distM)
plot(dianaClustering)
plot(pca$x, col = cutree(dianaClustering, k = 3) + 1, main="diana - clustering into 3 groups")

#Visualisation of trees and improvements

#An example of function for tree pruning and label modification
trim = function(node) {
  if(attr(node, "height") < 0.3) attr(node, "height") = 0.3
  if(is.leaf(node)) {
    if(attr(node, "height") == 0.3) attr(node, "height") = 0.2
    if(attr(node,"x.member") > 1)  attr(node, "label") = paste(sub("Branch", "Cluster", attr(node, "label")), ": ", attr(node,"x.member"), " members", sep="")
    else attr(node, "label") = paste(sub("Branch", "Cluster", attr(node, "label")), ": ", attr(node,"x.member"), " member", sep ="")
  }
  return(node)
}
trimmedTree = dendrapply(cut(as.dendrogram(agnesClustSingle), h = 0.7)$upper, trim)

plot(trimmedTree, main = "Trimmed clustering tree", ylim = c(-0.1, 1.9), edgePar = list(lwd = 1), center = T,
     nodePar = list(col=3:2, cex=c(4.0, 1.75), pch= 21:22, bg= c("light blue", "pink"), lab.cex = 1.2, lab.col = "Red"))

#When is it better to use hierarchical clustering?

plot(mlbench.smiley())
smileyData = mlbench.smiley()$x

#a shot at PAM
pamClustering = pam(smileyData, k = 4)
plot(pamClustering, labels = 2, stand = T, cor = T, shade = T)  #better to plot by hand...

plot(smileyData, main = "PAM clustering for the smiley data", col = c("Red", "Green", "Black", "Blue")[pamClustering$clustering])

#a shot at kmeans
kmeansClustering = kmeans(smileyData, centers = 4, iter.max = 50, nstart = 150)
plot(smileyData, main = "kmeans clustering for the smiley data", col = c("Red", "Green", "Black", "Blue")[kmeansClustering$cluster])

distM = dist(smileyData, method = "euclidean")
agnesClustSingle = agnes(distM, method = "single")
plot(smileyData, main = "single link clustering for the smiley data", 
     col = c("Red", "Green", "Black", "Blue")[cutree(agnesClustSingle, k = 4)])

#Another such example:
plot(mlbench.spirals(500,2,0.03))
spiralsData = mlbench.spirals(500,2,0.03)$x

pamClustering = pam(spiralsData, k = 2)
plot(spiralsData, main = "PAM clustering for the spirals data", col = c("Red", "Green", "Black", "Blue")[pamClustering$clustering])

kmeansClustering = kmeans(spiralsData, centers = 2, iter.max = 50, nstart = 150)
plot(spiralsData, main = "kmeans clustering for the spirals data", col = c("Red", "Green", "Black", "Blue")[kmeansClustering$cluster])

distM = dist(spiralsData, method = "euclidean")
agnesClustSingle = agnes(distM, method = "single")
plot(spiralsData, main = "single link clustering for the spirals data", 
     col = c("Red", "Green", "Black", "Blue")[cutree(agnesClustSingle, k = 2)])

rm(list = ls()); gc()

#Tasks:
# Take the ionosphere data from previous labs. For a moment "forget" about decision, i.e., mask the class column.
# Using whatever tool you like do the following:
# Task 1 : Using k-means or any of its modifications (PAM, clara)  and appropriate measures determine 
# the optimal number of clusters for ionosphere data. Then, with selected optimal clustering unmask the decision column 
# and check uniformity of clusters w.r.t decision.
# Task 2: Using agglomerative hierarchical clustering produce the hierarchy for ionosphere data, without decision. 
# Take the cross-section of the tree that results in the number of clusters that was optimal in Task 1. 
# Check uniformity of these clusters w.r.t decision.


install.packages("cluster")
install.packages("proxy")
install.packages("foreign")

library(cluster)
library(proxy)
library(foreign)


ionosphere = read.arff("/home/robert/Code/SUS/11/ionosphere.arff")
class = ionosphere$class
ionosphere = ionosphere[-c(2, ncol(ionosphere))]


silhouetteVec = rep(0,20)
for(i in 2:20)  {
  clustering = kmeans(ionosphere, centers = i, iter.max = 20, nstart = 150)
  silhouetteVec[i] = mean(silhouette(clustering$clust, dist(ionosphere))[,3])
  cat(i, silhouetteVec[i], "\n", sep="\t")
}
cat("According to silhouette coefficient the  division into  ", which.max(silhouetteVec), " groups is optimal \n", sep="")


pca = prcomp(ionosphere)
clustering = kmeans(ionosphere, centers = which.max(silhouetteVec), iter.max = 20, nstart = 150)
plot(pca$x, col = (1:which.max(silhouetteVec) + 1)[clustering$cluster])

for(i in 1:4) {
  g = length(intersect(which(clustering$cluster == i), which(class == "g")))
  b = length(intersect(which(clustering$cluster == i), which(class == "b")))
  cat(g, b, "\n", sep=" ")
}


distM = dist(ionosphere, method = "euclidean")

agnesClustComplete = agnes(distM, method = "complete")
plot(pca$x, col = cutree(agnesClustComplete, k = which.max(silhouetteVec)) + 1)
cluster = cutree(agnesClustComplete, k = which.max(silhouetteVec))

for(i in 1:4) {
  g = length(intersect(which(cluster == i), which(class == "g")))
  b = length(intersect(which(cluster == i), which(class == "b")))
  cat(g, b, "\n", sep=" ")
}
