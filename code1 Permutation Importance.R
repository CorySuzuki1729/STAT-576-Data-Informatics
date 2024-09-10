# Load necessary libraries
library(caret)  # For model training and accuracy calculation
library(dplyr)  # For data manipulation

# Load the Iris dataset
data(iris)

# Split the data into features and target variable
X <- iris %>% select(-Species)
y <- iris$Species

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(42)  # For reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Train a logistic regression model using caret
model <- train(X_train, y_train, method = "multinom", trControl = trainControl(method = "none"))

# Evaluate the model's performance on the test set (baseline accuracy)
baseline_preds <- predict(model, X_test)
baseline_accuracy <- mean(baseline_preds == y_test)
cat(sprintf("Baseline Accuracy: %.4f\n", baseline_accuracy))

# Define the permutation importance function
permutation_importance <- function(model, X_test, y_test, metric = "Accuracy", n_repeats = 10) {
  # Compute the baseline accuracy
  baseline_preds <- predict(model, X_test)
  baseline_score <- postResample(baseline_preds, y_test)[[metric]]
  
  # Initialize a list to store importance scores
  importance_scores <- vector("list", ncol(X_test))
  names(importance_scores) <- colnames(X_test)
  
  # Loop over each feature in X_test
  for (col in colnames(X_test)) {
    scores <- c()
    
    # Repeat the permutation n_repeats times
    for (i in 1:n_repeats) {
      # Shuffle the current feature
      X_test_shuffled <- X_test
      X_test_shuffled[[col]] <- sample(X_test[[col]])
      
      # Make predictions with the shuffled feature and compute accuracy
      shuffled_preds <- predict(model, X_test_shuffled)
      shuffled_score <- postResample(shuffled_preds, y_test)[[metric]]
      
      # Compute the performance drop
      score_drop <- baseline_score - shuffled_score
      scores <- c(scores, score_drop)
    }
    
    # Store the average importance score for the feature
    importance_scores[[col]] <- mean(scores)
  }
  
  # Return the importance scores
  return(importance_scores)
}

# Compute permutation importance
importances <- permutation_importance(model, X_test, y_test)

# Display feature importances
cat("\nFeature Importances (Permutation):\n")
for (feature in names(importances)) {
  cat(sprintf("%s: %.4f\n", feature, importances[[feature]]))
}
