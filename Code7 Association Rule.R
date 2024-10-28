# Load necessary libraries
library(arules)

# Load the dataset
df <- read.csv("food_ratings.csv")

# Remove non-binary columns (like 'Item' if present)
df <- df[, -1]
df <- df[, sapply(df, function(col) length(unique(col)) > 1)]

df <- df > 0 # Binarizes the data: values > 3 become TRUE (1), others become FALSE (0)

# Remove columns with only one unique value
# Convert to transactions
df <- as(df, "transactions")
summary(df)

# Step 1: Generate the rules using support and confidence
rules <- apriori(df, parameter = list(supp = 0.2, conf = 0.8, target = "rules"), maxlen = 15)
inspect(rules)
# Step 2: Filter rules based on lift greater than 2
rules <- subset(rules, subset = lift > 1.9)

# Step 3: Inspect the filtered rules
final_rule <- inspect(rules)

