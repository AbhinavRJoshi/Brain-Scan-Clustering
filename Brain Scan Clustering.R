#Reading the healthy cluster csv  
healthy = read.csv("healthy.csv", header = FALSE)

#Converting the dataframe into a matrix
healthymatrix = as.matrix(healthy)

#Displaying the image of the healthy brain
image(healthymatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

#Converting the matrix into a vector
health.vector = as.vector(healthymatrix)

#In order to figure out how many clusters we should create we are going to use the elbow method
mydata = as.data.frame(health.vector)

#creating a vector containing the variance for each k number of cluster
wss = (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) 
  wss[i] = sum(kmeans(mydata,centers=i)$withinss)

#Plotting the variance against the number of cluster
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#From the plot we can identify that the elbow is at 5
k=5

#Setting a random seed before k-means clustering to ensure reproducibility
set.seed(144)

#Clustering health.vector into k separate clusters and rearranging into the shape of a matrix
KMC = kmeans(health.vector, centers = k, iter.max = 1000)
healthyclusters = KMC$cluster
dim(healthyclusters) = c(566,646)

#Displaying the image of the tumor with each cluster assigned a different color
image(healthyclusters, axes = FALSE, col = heat.colors(k))

#Reading, transposing and converting the tumor mri file into a matrix
tumor = read.csv("tumor.csv",header = FALSE)
tumor = as.data.frame(t(tumor))
tumormatrix = as.matrix(tumor)

#Displaying the MRI image of the brain with the tumor scan. The dark section in the front lobe represents the tumor
image(tumormatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

#Converting the matrix into a vector to create clusters
tumorvector = as.vector(tumormatrix)

#To use the clusters to predict clusters for the tumor MRI we will load the FlexClust library and create a kcca objec
library(flexclust)
KMC.KCCA = as.kcca(KMC, health.vector)

#Using the kkca object we will predict the clusters for the tumor and reshape it into the same dimensions as a matrix
tumorcluster = predict(KMC.KCCA, newdata = tumorvector)
dim(tumorcluster) = c(nrow(tumormatrix),ncol(tumormatrix))


#Displaying the MRI image of the brain with the tumor scan with each cluster being represented by a different hear color
image(tumorcluster, axes = FALSE, col = heat.colors(k),useRaster = TRUE)

#As seen from the image the tumor in the front lobe of the brain appears as a different color from the rest of brain matter making it easier for doctors to identify the position and dimensions of the tumor
