

# Importing dataset
dataset = read.csv(file.choose())
x = dataset[,4:5]

#################################################################################

# CLUSTERING : K-MEANS

# Using Elbow Method to find optimal no. of clusters
set.seed(6)
wcss = vector()
for(i in 1:10) wcss[i] = sum(kmeans(x, i)$withinss)
plot(1:10, wcss, "b", 
     main = paste("Cluster of clients"),
     xlab = 'Number of clusters',
     ylab = "wcss")


# Applying k-means to mall dataset
set.seed(29)
kmeans = kmeans(x , 5, iter.max = 300, nstart = 10)

# Visualizing the clusters
library(cluster)
clusplot(x, 
         kmeans$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste("Cluster of clients"),
         xlab = 'Annual Order Quantity',
         ylab = "Spending Score")

################################################################################

# CLUSTERING : HIERARCHICAL CLUSTERING

# Using Dendogram to find optimal no. of clusters
dendogram = hclust(dist(x, method = 'euclidean'), method = 'ward.D')
plot(dendogram,
     main = paste('Dendogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')

# Fitting hierarchical clustering to mall dataset
hc = hclust(dist(x, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)
y_hc

# Visualising the clusters
library(cluster)
clusplot(x, 
         y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste("Cluster of clients"),
         xlab = 'Annual Order Quantity',
         ylab = "Spending Score")



