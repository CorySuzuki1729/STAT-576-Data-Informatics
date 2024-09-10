# Load necessary libraries
library(datasets)     # For iris dataset
library(dplyr)        # Data manipulation
library(ggplot2)      # For plotting
library(reshape2)     # For reshaping data
library(skimr)
library(caret)     # For feature selection

# Load the Iris dataset
data(iris)

# Display the first few rows of the data
head(iris)

# Summary statistics of the data
skim(iris)

# Remove target column "Species" to apply variance threshold only on features
X <- iris %>% select(-Species)

################################################################################
##
## 1. Variance Threshold
##
################################################################################

# Calculate the variance of each feature
feature_variances <- apply(X, 2, var)

# Filter out columns with variance less than the threshold
threshold_variance <- 0.2
X_high_variance <- X[, feature_variances > threshold_variance]

# Show shape of data after applying variance threshold
cat("Shape of data after Variance Threshold:", dim(X_high_variance), "\n")
head(X_high_variance)

################################################################################
##
## 2. Correlation
##
################################################################################

# Compute the correlation matrix
correlation_matrix <- cor(X)

# Print the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Define a threshold for highly correlated features
cor_threshold <- 0.8

# Loop through the correlation matrix and find highly correlated pairs
for (i in 1:(ncol(correlation_matrix) - 1)) {
  for (j in (i + 1):ncol(correlation_matrix)) {
    corr_value <- correlation_matrix[i, j]
    if (abs(corr_value) > cor_threshold) {
      cat(sprintf("%s and %s: %.4f\n", colnames(correlation_matrix)[i], colnames(correlation_matrix)[j], corr_value))
    }
  }
}

# Visualize the correlation matrix using ggplot2
# Melt the correlation matrix for ggplot2 compatibility
corr_matrix_melt <- melt(correlation_matrix)

# Plot the heatmap
ggplot(corr_matrix_melt, aes(Var1, Var2, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  coord_fixed() + 
  labs(title = "Correlation Matrix Heatmap", x = "", y = "")

################################################################################
##
## 3. Chi-2
##
################################################################################

## Discretize numerical features using cut
X_d <- X
X_d$Sepal.Length <- cut(X_d$Sepal.Length, breaks = 3, labels = c("low", "medium", "high"))
X_d$Sepal.Width <- cut(X_d$Sepal.Width, breaks = 3, labels = c("low", "medium", "high"))
X_d$Petal.Length <- cut(X_d$Petal.Length, breaks = 3, labels = c("low", "medium", "high"))
X_d$Petal.Width <- cut(X_d$Petal.Width, breaks = 3, labels = c("low", "medium", "high"))

# Perform Chi-Square test for each feature against the target (Species)
chi_sq_sepal_length <- chisq.test(table(X_d$Sepal.Length, iris$Species))
chi_sq_sepal_width <- chisq.test(table(X_d$Sepal.Width, iris$Species))
chi_sq_petal_length <- chisq.test(table(X_d$Petal.Length, iris$Species))
chi_sq_petal_width <- chisq.test(table(X_d$Petal.Width, iris$Species))

# Extract p-values from the Chi-Square tests
p_values <- c(
  chi_sq_sepal_length$p.value,
  chi_sq_sepal_width$p.value,
  chi_sq_petal_length$p.value,
  chi_sq_petal_width$p.value
)

# Combine feature names and p-values
features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
chi_sq_results <- data.frame(Feature = features, P_Value = p_values)

# Rank features by p-value (lower p-value means more significant)
chi_sq_results <- chi_sq_results[order(chi_sq_results$P_Value), ]

# Print the ranked features
print(chi_sq_results)

################################################################################
##
## 4. ANOVA
##
################################################################################

# Perform ANOVA for each feature
anova_sepal_length <- summary(aov(Sepal.Length ~ Species, data = iris))
anova_sepal_width  <- summary(aov(Sepal.Width ~ Species, data = iris))
anova_petal_length <- summary(aov(Petal.Length ~ Species, data = iris))
anova_petal_width  <- summary(aov(Petal.Width ~ Species, data = iris))

# Extract F-values
f_values <- c(
  anova_sepal_length[[1]][["F value"]],
  anova_sepal_width[[1]][["F value"]],
  anova_petal_length[[1]][["F value"]],
  anova_petal_width[[1]][["F value"]]
)

# Combine feature names and F-values
features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
anova_results <- data.frame(Feature = features, F_Value = f_values)

# Rank features based on F-value and select the top k features (e.g., k = 2)
k <- 3
selected_features <- head(anova_results[order(-anova_results$F_Value), ], k)

# Print the selected features
print(selected_features)

