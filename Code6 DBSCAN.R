# Load necessary libraries
library(dbscan)
library(ggplot2)
library(datasets)

# Load Iris dataset
data(iris)
X <- iris[, -5]  # Use all features except the species column

# Apply DBSCAN
db <- dbscan(X, eps = 0.5, minPts = 5)

# Add clusters to the original data
iris$Cluster <- as.factor(db$cluster)

# Plot DBSCAN Clustering (visualized with Sepal Length vs Sepal Width)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size=3) +
  labs(title = "DBSCAN Clustering on Iris Dataset (Visualized with Sepal Length vs Sepal Width)",
       x = "Sepal Length (cm)", y = "Sepal Width (cm)") +
  theme_minimal()
