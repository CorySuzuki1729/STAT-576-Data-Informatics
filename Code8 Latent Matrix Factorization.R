library(NMF)
library(readxl)
library(dplyr)

df <- read_excel("Food_2024_Fall_A.xlsx", sheet = "Sheet1")
ratings_matrix <- as.matrix(df[, -1])

# Transpose the matrix so that rows represent users and columns represent items
ratings_matrix <- t(ratings_matrix)

# Replace 0s with NA to treat them as missing values for mean imputation in NMF
ratings_matrix[ratings_matrix == 0] <- NA

# Impute missing values with the mean of each column for NMF
ratings_matrix_imputed <- apply(ratings_matrix, 2, function(col) {
  ifelse(is.na(col), mean(col, na.rm = TRUE), col)
})

# ----- Non-negative Matrix Factorization (NMF) -----
# Set the number of latent factors
k <- 2  # Adjust as needed for dimensionality

# Apply NMF
nmf_model <- nmf(ratings_matrix_imputed, rank = k, method = "brunet", nrun = 10, seed = 42)
W <- basis(nmf_model)  # User latent matrix
H <- coef(nmf_model)   # Item latent matrix

# Reconstruct the matrix using NMF factors
predicted_ratings_nmf <- W %*% H

# ----- Custom ALS Implementation -----
# ALS Parameters
num_users <- nrow(ratings_matrix)
num_items <- ncol(ratings_matrix)
lambda <- 0.1  # Regularization parameter
n_iter <- 5000  # Number of iterations

# Initialize user and item latent matrices with small random values
set.seed(42)
U <- matrix(rnorm(num_users * k, sd = 0.01), nrow = num_users, ncol = k)
V <- matrix(rnorm(num_items * k, sd = 0.01), nrow = num_items, ncol = k)

# ALS Optimization Process
for (iteration in 1:n_iter) {
  # Update User Matrix U
  for (i in 1:num_users) {
    rated_items <- !is.na(ratings_matrix[i, ])
    if (sum(rated_items) > 0) {
      V_i <- V[rated_items, ]
      R_i <- ratings_matrix[i, rated_items]
      U[i, ] <- solve(t(V_i) %*% V_i + lambda * diag(k)) %*% t(V_i) %*% R_i
    }
  }
  
  # Update Item Matrix V
  for (j in 1:num_items) {
    rated_users <- !is.na(ratings_matrix[, j])
    if (sum(rated_users) > 0) {
      U_j <- U[rated_users, ]
      R_j <- ratings_matrix[rated_users, j]
      V[j, ] <- solve(t(U_j) %*% U_j + lambda * diag(k)) %*% t(U_j) %*% R_j
    }
  }
  
  # Compute Reconstruction Error
  prediction <- U %*% t(V)
  error <- sum((ratings_matrix - prediction)^2, na.rm = TRUE) + lambda * (sum(U^2) + sum(V^2))
  
  # Print error every few iterations
  if (iteration %% 100 == 0 || iteration == 1) {
    cat("Iteration:", iteration, "Error:", error, "\n")
  }
}

# Final Prediction Matrix for ALS
predicted_ratings_als <- U %*% t(V)
cat("Predicted Ratings Matrix (ALS):\n")
print(predicted_ratings_als)


# ----- New User Prediction Example (ALS) -----
# Use the same new user ratings as previously defined
new_user_ratings <- c(4, NA, 3, NA, 2, 5, NA, NA, 4, NA, NA, 1, 5, 2, NA, 3, NA, 4, 5, NA, 2, 4, NA, 5, 3, NA, 4, NA, NA, 5)

# Fill NA with column means for the items for ALS
mean_item_ratings <- colMeans(ratings_matrix, na.rm = TRUE)
new_user_filled <- ifelse(is.na(new_user_ratings), mean_item_ratings, new_user_ratings)

# Solve for the new user's latent factors using the item matrix V
# Ensure that V is used directly, not transposed, to match dimensions
new_user_vector <- solve(t(V) %*% V + lambda * diag(k)) %*% t(V) %*% new_user_filled

# Predict ratings for the new user
new_user_predicted_ratings <- t(new_user_vector) %*% t(V)
cat("\nPredicted Ratings for New User (ALS):\n")
print(new_user_predicted_ratings)
