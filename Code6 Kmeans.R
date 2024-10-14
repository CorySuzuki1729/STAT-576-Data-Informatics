# Install and load required libraries
if (!require(mclust)) {
  install.packages("mclust")
}
library(mclust) # For Adjusted Rand Index (ARI)
library(cluster) # For silhouette
library(factoextra) # For visualization
library(datasets) # For Iris dataset
library(caret) # For confusion matrix

# Load the Iris dataset
data(iris)
X <- iris[, -5] # Remove species label, keeping only the features
y_true <- iris$Species # True labels

# Apply K-means clustering with k=3
set.seed(42)
kmeans_result <- kmeans(X, centers=3, nstart=10)

# Compare clusters with true labels using Adjusted Rand Index (ARI)
ari <- adjustedRandIndex(kmeans_result$cluster, as.numeric(as.factor(y_true)))
cat("Adjusted Rand Index (ARI):", ari, "\n")

# Confusion matrix to compare clusters with true labels
conf_mat <- confusionMatrix(as.factor(kmeans_result$cluster), as.factor(as.numeric(as.factor(y_true))))

# Print confusion matrix
print(conf_mat)

# Plot the clusters using Petal Length and Petal Width
fviz_cluster(kmeans_result, data = X, geom = "point", 
             stand = FALSE, ellipse.type = "convex", show.clust.cent = TRUE,
             ggtheme = theme_minimal(), main = "K-means Clustering on Iris Data")
