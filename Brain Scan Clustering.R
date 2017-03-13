#Reading the healthy cluster csv  
healthy = read.csv("healthy.csv", header = FALSE)

healthymatrix = as.matrix(healthy)

image(healthymatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

health.vector = as.vector(healthymatrix)

mydata <- health.vector
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

set.seed(1)

KMC = kmeans(health.vector, centers = k, iter.max = 1000)

healthyclusters = KMC$cluster
dim(healthyclusters) = c(566,646)

image(healthyclusters, axes = FALSE, col = rainbow(k))

tumor = read.csv("tumor.csv",adimheader = FALSE)
tumormatrix = as.matrix(tumor)
tumorvector = as.vector(tumormatrix)

library(flexclust)

KMC.KCCA = as.kcca(KMC, health.vector)
tumorcluster = predict(KMC.KCCA, newdata = tumorvector)
dim(tumorcluster) = c(nrow(tumormatrix),ncol(tumormatrix))
image(tumorcluster, axes = FALSE, col = rainbow(k))
