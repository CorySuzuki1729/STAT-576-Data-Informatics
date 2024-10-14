library(cluster) # For silhouette score and Agglomerative clustering
library(dendextend) # For plotting dendrogram
library(datasets) # For Iris dataset
library(caret) # For confusion matrix
library(factoextra) # For cluster visualization

# Load the Iris dataset
data(iris)
X <- iris[, -5] # Remove species label, keeping only the features
y_true <- iris$Species # True labels

# Perform Agglomerative Clustering
dist_matrix <- dist(X) # Compute the distance matrix
hc <- hclust(dist_matrix, method="complete") # Hierarchical clustering using single method

# Cut the dendrogram to create 3 clusters
clusters <- cutree(hc, k=3)

# Compare clusters with true labels using Adjusted Rand Index (ARI)
if (!require(mclust)) {
  install.packages("mclust")
}
library(mclust)
ari <- adjustedRandIndex(clusters, as.numeric(as.factor(y_true)))
cat("Adjusted Rand Index (ARI):", ari, "\n")

# Confusion matrix to compare clusters with true labels
conf_mat <- confusionMatrix(as.factor(clusters), as.factor(as.numeric(as.factor(y_true))))
print(conf_mat)

# Plot the dendrogram
plot(as.dendrogram(hc), main = "Dendrogram for Iris Data (Agglomerative Clustering)",
     xlab = "Samples", ylab = "Distance")

# Plot the clusters using Petal Length and Petal Width
fviz_cluster(list(data = X, cluster = clusters), geom = "point", 
             stand = FALSE, ellipse.type = "convex", show.clust.cent = FALSE,
             ggtheme = theme_minimal(), main = "Agglomerative Clustering on Iris Data")


