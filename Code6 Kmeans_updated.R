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

# ----- Elbow Method for finding the optimal number of clusters -----
# Compute total within-cluster sum of squares for k = 1 to 10
wss <- function(k) {
  kmeans(X, k, nstart = 10)$tot.withinss
}

# Apply the elbow method and store WSS values for different k
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# Plot the elbow method results
plot(k_values, wss_values, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters (k)", ylab="Total within-cluster sum of squares (WSS)",
     main="Elbow Method for K-Means")
grid()

# ----- Silhouette Method for finding the optimal number of clusters -----
# Calculate silhouette width for different values of k
sil_width <- numeric(9)  # Starting from k=2 to k=10
for (k in 2:10) {
  kmeans_result <- kmeans(X, centers = k, nstart = 10)
  ss <- silhouette(kmeans_result$cluster, dist(X))
  sil_width[k-1] <- mean(ss[, 3])  # Average silhouette width
}

# Plot the silhouette method results
plot(2:10, sil_width, type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters (k)", ylab="Average silhouette width",
     main="Silhouette Method for K-Means")
grid()
