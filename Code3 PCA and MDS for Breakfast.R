# Load necessary libraries
library(ggplot2)
library(readr)
library(MASS)  # For Stress-based MDS

# Load the dataset
data <- read_csv("Breakfast.csv")

# Assuming the first column is the label and the rest are features
y <- data[[1]]    # Labels (First column)
X <- data[, -1]   # Features (Remaining columns)

# Perform PCA to reduce to 2 components
pca_result <- prcomp(X, center = TRUE, scale. = FALSE)
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$label <- y

# Perform Stress-based MDS (non-metric MDS) to reduce to 2 components
original_distances <- dist(X)  # Calculate the distance matrix
mds_result <- isoMDS(original_distances, k = 2)  # Perform non-metric MDS
mds_data <- as.data.frame(mds_result$points)  # Extract MDS coordinates
mds_data$label <- y
colnames(mds_data) <- c("MDS1", "MDS2", "label")

# PCA Plot with larger points and labels
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, label = label)) +
  geom_point(aes(color = factor(label)), size = 3) +  # Increase point size
  geom_text(vjust = -1, size = 3) +  # Increase label size
  labs(title = "PCA Scatter Plot with First Two Components") +
  theme_minimal()

# Show the PCA plot
print(pca_plot)

# Stress-based MDS Plot with larger points and labels
mds_plot <- ggplot(mds_data, aes(x = MDS1, y = MDS2, label = label)) +
  geom_point(aes(color = factor(label)), size = 3) +  # Increase point size
  geom_text(vjust = -1, size = 3) +  # Increase label size
  labs(title = "Stress-based MDS Scatter Plot with First Two Components") +
  theme_minimal()

# Show the Stress-based MDS plot
print(mds_plot)

# Print the stress value from the MDS result
cat("Stress value from the MDS:", mds_result$stress, "\n")
