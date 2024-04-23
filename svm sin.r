library(ggplot2)
library(dplyr)
library(stringr)
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
library(ggplot2)
library(dplyr)
library(stringr)
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


# SVM SINDHU BUGGANA

# Load required package
library(reshape2)

library(e1071)
#' my_data' contains  preprocessed dataset
head(my_data, n = 20)

summary(my_data) # Summarizing columns

nrow(my_data) # missing values
my_data <- na.omit(my_data)
nrow(my_data) # check after missing values
# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(my_data), 0.7 * nrow(my_data)) # randomly samples
train_data <- my_data[train_index, ]
test_data <- my_data[-train_index, ]
# Train the SVM model
svm <- svm(Obesity ~ ., data = train_data, kernel = "linear")

# Make predictions on the testing set
predictions <- predict(svm, newdata = test_data)

# Evaluate the performance of the SVM model
confusion_matrix <- table(predictions, test_data$Obesity) * 100
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Calculate additional metrics
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]

sensitivity <- true_positives / (true_positives + false_negatives)
specificity <- true_negatives / (true_negatives + false_positives)

cat("SVM Accuracy:", accuracy, "\n")
cat("SVM Precision:", precision, "\n")
cat("SVM Recall:", recall, "\n")
cat("SVM F1-score:", f1_score, "\n")
cat("SVM Sensitivity:", sensitivity, "\n")
cat("SVM Specificity:", specificity, "\n")



# Convert confusion matrix to data frame
confusion_matrix_df <- as.data.frame(confusion_matrix)
str(confusion_matrix)
dim(confusion_matrix)

# Assign column names
colnames(confusion_matrix_df) <- c("Non-Obese", "Obese")

# Assign row names
# rownames(confusion_matrix_df) <- c("Non-Obese", "Obese")

# Melt the data frame for visualization
confusion_matrix_melted <- melt(confusion_matrix_df)
colnames(confusion_matrix_melted)
str(confusion_matrix_melted)

# Convert Gender to numeric
train_data$Gender <- as.numeric(train_data$Gender)


# Calculate feature importance
feature_importance <- abs(t(svm$coefs) %*% svm$SV)
print(feature_importance) # Check if feature_importance is not NULL
feature_importance <- feature_importance / sum(feature_importance)
print(feature_importance) # Check if feature_importance is not NULL


# Get feature names from the training data
feature_names <- names(train_data)[!names(train_data) %in% c("Obesity")]

# Ensure feature_names and feature_importance have the same length
feature_importance <- feature_importance[1:length(feature_names)]
print(feature_importance) # Check if feature_importance is not NULL

# Create the feature importance data frame
feature_importance_df <- data.frame(
    Feature = feature_names,
    Importance = feature_importance
)

ggplot(feature_importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "SVM Feature Importance", x = "Feature", y = "Importance") +
    theme_minimal()
