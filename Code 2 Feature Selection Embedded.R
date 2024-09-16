# Load necessary libraries
library(readr)
library(caret)
library(randomForest)
library(glmnet)
library(DiceKriging)

# Load the Diabetes dataset from the saved CSV file
diabetes_data <- read_csv("diabetes_data.csv")

# Preview the data
print(head(diabetes_data))

# Separate features (X) and target (y)
X <- as.matrix(diabetes_data[, -ncol(diabetes_data)])  # Exclude the target column
y_continuous <- diabetes_data$target  # Continuous target column (last column)

# Convert continuous target to binary (e.g., values greater than the mean are 1, others are 0)
y <- ifelse(y_continuous > mean(y_continuous), 1, 0)

# Standardize the features
preProc <- preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preProc, X)

# --- 1. Logistic Regression with p-values (Select Top 5) ---
print("1. Logistic Regression Feature Selection (Top 5 Features by p-value)")
log_reg_model <- glm(y ~ ., data = as.data.frame(X_scaled), family = binomial)
p_values <- summary(log_reg_model)$coefficients[, 4]  # Extract p-values

# Select the top 5 features based on p-values
top_5_log_reg <- order(p_values)[1:5]
selected_features_log_reg <- names(p_values)[top_5_log_reg]
print(paste("Top 5 features by Logistic Regression (p-value):", selected_features_log_reg))

# --- 2. Random Forest (Select Top 5 Features) ---
print("2. Random Forest Feature Selection (Top 5 Features by Importance)")
rf_model <- randomForest(as.factor(y) ~ ., data = as.data.frame(X_scaled), importance = TRUE)
importance_rf <- importance(rf_model)

# Select the top 5 features based on MeanDecreaseGini
top_5_rf <- order(importance_rf[, "MeanDecreaseGini"], decreasing = TRUE)[1:5]
selected_features_rf <- rownames(importance_rf)[top_5_rf]
print(paste("Top 5 features by Random Forest:", selected_features_rf))

# --- 3. Gaussian Process with ARD using DiceKriging (Select Top 5 Features) ---
print("3. Gaussian Process Feature Selection (Top 5 Features by ARD using DiceKriging)")

# Fit Gaussian Process with ARD (Automatic Relevance Determination)
gp_model <- km(design = as.data.frame(X_scaled), response = as.factor(y), covtype = "gauss", 
               nugget.estim = TRUE, control = list(trace = FALSE))

# Extract the length scales from the fitted model (inverse of scales, smaller means more relevant)
length_scales_gp <- gp_model@covariance@range.val

# Select the top 5 features based on smallest length scales (most relevant)
top_5_gp <- order(length_scales_gp)[1:5]
selected_features_gp <- colnames(X_scaled)[top_5_gp]
print(paste("Top 5 features by Gaussian Process (ARD):", selected_features_gp))

# --- 4. Lasso Regression (Select Top 5 Features) ---
print("4. Lasso Regression Feature Selection (Top 5 Features by Coefficient Magnitude)")
lasso_model <- cv.glmnet(X_scaled, as.factor(y), alpha = 1, family = "binomial")
coef_lasso <- abs(coef(lasso_model, s = "lambda.min"))

# Select the top 5 features based on coefficient magnitudes
top_5_lasso <- order(coef_lasso[-1], decreasing = TRUE)[1:5]  # Ignore intercept (first coefficient)
selected_features_lasso <- rownames(coef_lasso)[top_5_lasso + 1]  # Add 1 to account for intercept
print(paste("Top 5 features by Lasso Regression:", selected_features_lasso))
