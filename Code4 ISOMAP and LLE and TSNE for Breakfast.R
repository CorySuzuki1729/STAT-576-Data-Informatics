# Load necessary libraries
library(ggplot2)
library(vegan)      # For Isomap
library(Rtsne)      # For t-SNE
library(MASS)       # For MDS
library(lle)        # For LLE

# Load the dataset
data <- read.csv("Breakfast.csv")

# Assuming the first column is the label and the rest are features
y <- data[, 1]  # Labels (First column)
X <- as.matrix(data[, -1])  # Features (Remaining columns)

# If the labels are categorical, convert them to numeric values
if (is.factor(y)) {
  y_encoded <- as.numeric(y)
} else {
  y_encoded <- y
}

# Perform PCA
pca_result <- prcomp(X, scale. = TRUE)$x[, 1:2]

# Perform MDS
mds_result <- isoMDS(dist(X), k = 2)$points

# Perform Isomap using the vegan package
isomap_result <- isomap(dist(X), k = 10, ndim = 2)$points  # k is the number of nearest neighbors

# Perform LLE using the 'lle' package
lle_result <- lle(X, m = 2, k = 10)$Y  # m = 2 dimensions, k = 10 nearest neighbors

# Perform t-SNE
tsne_result <- Rtsne(X, dims = 2, perplexity = 3)$Y

# Function to create scatter plots showing all labels
plot_scatter_with_all_labels <- function(data, method_name, y) {
  data <- data.frame(X1 = data[, 1], X2 = data[, 2], Label = as.factor(y_encoded))
  
  ggplot(data, aes(x = X1, y = X2, color = Label)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_text(aes(label = Label), vjust = -0.5, hjust = 0.5, size = 3) +  # Add all labels
    labs(title = paste(method_name, "Scatter Plot with First Two Components"),
         x = paste("First", method_name, "Dimension"),
         y = paste("Second", method_name, "Dimension")) +
    scale_color_viridis_d() +
    theme_minimal()
}

# Plot each method's result with all labels
plot_scatter_with_all_labels(pca_result, 'PCA', y)
plot_scatter_with_all_labels(mds_result, 'MDS', y)
plot_scatter_with_all_labels(isomap_result, 'Isomap', y)
plot_scatter_with_all_labels(lle_result, 'LLE', y)
plot_scatter_with_all_labels(tsne_result, 't-SNE', y)
