library(ggplot2)
library(dplyr)
library(stringr)
# for graph
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot) # Load the rpart.plot package
library(rlang) # Load the rlang package
library(caret) # Load the caret packag
rm(list = ls())

setwd("/Users/sindh/Downloads/513-B-KDDM")
my_data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors = F)
head(my_data)

table(my_data$CALC)
table(my_data$FAVC)
table(my_data$SCC)
table(my_data$SMOKE)
table(my_data$family_history_with_overweight)
table(my_data$CAEC)
table(my_data$MTRANS)

change_cols <- c("FAVC", "SCC", "SMOKE", "family_history_with_overweight")

for (cols in change_cols) {
    my_data[, cols] <- gsub("yes", 1, my_data[, cols])
    my_data[, cols] <- gsub("no", 0, my_data[, cols])
    my_data[, cols] <- as.factor(my_data[, cols])
}

my_data$Gender <- gsub("Male", 0, my_data$Gender)
my_data$Gender <- gsub("Female", 1, my_data$Gender)
my_data$Gender <- as.factor(my_data$Gender)

my_data$MTRANS <- gsub("Bike", "Walking", my_data$MTRANS)
my_data$MTRANS <- gsub("Motorbike", "Automobile", my_data$MTRANS)

my_data$MTRANS <- gsub("Walking", 0, my_data$MTRANS)
my_data$MTRANS <- gsub("Automobile", 1, my_data$MTRANS)
my_data$MTRANS <- gsub("Public_Transportation", 2, my_data$MTRANS)
my_data$MTRANS <- as.factor(my_data$MTRANS)

my_data$CAEC <- gsub("Frequently", "Always", my_data$CAEC)
my_data$CAEC <- gsub("Sometimes", "no", my_data$CAEC)

my_data$CAEC <- gsub("Always", 1, my_data$CAEC)
my_data$CAEC <- gsub("no", 0, my_data$CAEC)
my_data$CAEC <- as.factor(my_data$CAEC)

my_data$Obesity <- 0
for (i in 1:nrow(my_data)) {
    if (grepl("Obesity_Type_", my_data$NObeyesdad[i])) {
        my_data$Obesity[i] <- 1
    }
    if (my_data$CALC[i] == "no") {
        my_data$CALC[i] <- 0
    } else {
        my_data$CALC[i] <- 1
    }
}
my_data$Obesity <- as.factor(my_data$Obesity)
my_data$CALC <- as.factor(my_data$CALC)

my_data <- my_data[, -c(3, 4, 17)]
col_names <- c(
    "Age", "Gender", "Alcohol", "Calorie_food", "Veggies", "Num_meals",
    "Calorie_monitor", "Smoking", "Water_quantity", "family_history", "Activity",
    "Tech_usage", "Food_bw_meals", "Transport", "Obesity"
)
names(my_data) <- col_names

View(my_data)
# write.csv (my_data, "obseitydataset.csv")

# CART SINDHU BUGGANA

# Load required packages
library(rpart)
library(class) # For additional classification functions
# install.packages("rpart") functions for building and visualizing CART

#' my_data' contains  preprocessed dataset
head(my_data, n = 20)

summary(my_data) # Summarizing columns

nrow(my_data) # missing values
my_data <- na.omit(my_data)
nrow(my_data) # check after missing values

# CART- Classification
# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(my_data), 0.7 * nrow(my_data)) # randomly samples
train_data <- my_data[train_index, ]
test_data <- my_data[-train_index, ]

# Train the CART model
cart_model <- rpart(Obesity ~ ., data = train_data, method = "class")

# Print the summary of the CART model
summary(cart_model)

# Make predictions on the testing set
predictions <- predict(cart_model, test_data, type = "class")

# Evaluate the performance of the CART model
confusion_matrix <- table(predictions, test_data$Obesity)
print(confusion_matrix)


# Calculate TP, TN, FP, and FN
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

# Calculate accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN) * 100

# Calculate sensitivity
sensitivity <- TP / (TP + FN)

# Calculate specificity
specificity <- TN / (TN + FP)

# Calculate precision
precision <- TP / (TP + FP)

# Calculate recall
recall <- TP / (TP + FN)

# Calculate F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("CART Accuracy:", accuracy, "%\n")
cat("CART Sensitivity:", sensitivity, "%\n")
cat("CART Specificity:", specificity, "%\n")
cat("CART Precision:", precision, "%\n")
cat("CART Recall:", recall, "%\n")
cat("CART F1-score:", f1_score, "%\n")

# Visualize the decision tree
plot(cart_model, margin = 0.1)
text(cart_model, use.n = TRUE, cex = 0.6)

# Plot feature importance graph
rpart.plot(cart_model, extra = 104, under = TRUE, cex = 0.8)
# output graph
