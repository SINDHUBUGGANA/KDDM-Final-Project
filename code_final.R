library(ggplot2)
library(dplyr)
library(stringr)
library(randomForest)
library(caret)
library(pROC)
library(class)
library(ada)
library(e1071)
library(gbm)
#install.packages("ROCR")
#install.packages("gbm")
library(gbm)
library(ROCR)
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
  my_data[,cols] <- gsub("yes", 1, my_data[,cols])
  my_data[,cols] <- gsub("no", 0, my_data[,cols])
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

my_data <- my_data[, -c(3,4,17)]
col_names <- c("Age", "Gender", "Alcohol", "Calorie_food", "Veggies", "Num_meals",
               "Calorie_monitor", "Smoking", "Water_quantity", "family_history", "Activity", 
               "Tech_usage", "Food_bw_meals", "Transport", "Obesity")
names(my_data) <- col_names
head(my_data)

# Splitting train dataset and test dataset
sample <- sample(c(TRUE,FALSE), nrow(my_data),replace=TRUE, prob=c(0.7,0.3))
train_dataset  <- my_data[sample, ]
test_dataset <- my_data[!sample, ]

################# K nearest neighbour ########################## 

# pred_test3 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 3)
# pred_test5 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 5)
# pred_test7 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 7)
# pred_test9 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 9)
# pred_test11 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 11)
# pred_test13 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 13)
# pred_test15 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 15)
# pred_test17 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 17)
# pred_test19 = knn(train_dataset[,-15],test_dataset[,-15], train_dataset$Obesity, k = 19)

k_values <- seq(3, 20, by = 2)
results <- lapply(k_values, function(k) {
  model <- train(Obesity ~ ., data = train_dataset, method = "knn", trControl = trainControl(method = "cv", number = 5), tuneGrid = data.frame(k = k))
  return(model$results$Accuracy)
})
optimal_k <- k_values[which.max(unlist(results))]
print(paste("Optimal k value:", optimal_k))
final_model <- train(Obesity ~ ., data = train_dataset, method = "knn", trControl = trainControl(method = "cv", number = 5), tuneGrid = data.frame(k = optimal_k))
predictions <- predict(final_model, newdata = test_dataset)
conf_matrix <- confusionMatrix(predictions, test_dataset$Obesity)
print(conf_matrix)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("K nearest neighbour Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_knn <- c(accuracy, precision, recall, f1_score, sensitivity, specificity)



# Predict probabilities for test dataset
knn_prob <- predict(final_model, newdata = test_dataset, type = "prob")

# Create a prediction object
knn_pred <- prediction(knn_prob[, "1"], test_dataset$Obesity)

# Create performance object
knn_perf <- performance(knn_pred, "tpr", "fpr")

# Plot ROC curve
plot(knn_perf, main = "ROC Curve for k-Nearest Neighbors (KNN) Model", col = "red", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "blue")


#### ######### logistic regrssion #########################
model <- glm(Obesity ~.,family=binomial,data=train_dataset)
summary(model)
predict_reg <- predict(model,test_dataset[,-15], type = "response")
predict_reg

binary_predictions <- ifelse(predict_reg > 0.5, 1, 0)
conf_matrix <- confusionMatrix(factor(binary_predictions), factor(test_dataset$Obesity))
print(conf_matrix)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("logistic regrssion Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_lr <- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

roc_curve <- roc(test_dataset$Obesity, predict_reg)
plot(roc_curve, main = "ROC Curve for Logistic Regression",col = "blue", lwd = 2,print.auc = TRUE, auc.polygon = TRUE, grid = TRUE)


############ Random forest #################################

# Initialize and train the Random Forest classifier
rf_classifier <- randomForest(Obesity ~ ., data = train_dataset, ntree = 100, importance = TRUE)

# Predictions
y_pred <- predict(rf_classifier, newdata = test_dataset[,-15])
y_test <- test_dataset$Obesity

# Evaluation
conf_matrix <- confusionMatrix(y_pred, y_test)
print(conf_matrix)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("Random forest Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_rf <- c(accuracy, precision, recall, f1_score, sensitivity, specificity)


# Predict probabilities for test dataset
rf_prob <- predict(rf_classifier, newdata = test_dataset, type = "prob")

# Create a prediction object
rf_pred <- prediction(rf_prob[, "1"], test_dataset$Obesity)

# Create performance object
rf_perf <- performance(rf_pred, "tpr", "fpr")

# Plot ROC curve
plot(rf_perf, main = "ROC Curve for Random Forest Classifier", col = "green", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "blue")

################## ada boost ###################################

ada_model <- ada_model(Obesity ~ ., data = train_dataset)
predictions <- predict(ada_model, newdata = test_dataset[,-15])
predictions

conf_matrix <- confusionMatrix(predictions, test_dataset$Obesity)
print(conf_matrix)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("ada boost Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_ada <- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

# Extract the probability column from ada_prob
ada_pred <- prediction(ada_prob[, 2], test_dataset$Obesity)

# Create performance object
ada_perf <- performance(ada_pred, "tpr", "fpr")

# Plot ROC curve
plot(ada_perf, main = "ROC Curve for AdaBoost Classifier", col = "red", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "blue")



### Naive Bayes ##############

# Train Naive Bayes Model
nb_model <- naiveBayes(train_dataset[,-15], train_dataset$Obesity)
# Predict using the Naive Bayes model
nb_predictions <- predict(nb_model, test_dataset[,-15])

# Evaluate Naive Bayes model
nb_confusion <- confusionMatrix(nb_predictions, test_dataset$Obesity)
print(nb_confusion)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("Naive Bayes model Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_naive <- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

# Predict probabilities for test dataset
nb_prob <- predict(nb_model, newdata = test_dataset[,-15], type = "raw")

# Create a prediction object
nb_pred <- prediction(nb_prob[, "1"], test_dataset$Obesity)

# Create performance object
nb_perf <- performance(nb_pred, "tpr", "fpr")

# Plot ROC curve
plot(nb_perf, main = "ROC Curve for Naive Bayes Classifier", col = "green", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "blue")

###### Gradient Boost ################
gbm_fit <- gbm(Obesity ~ ., data = train_dataset,
               distribution = "multinomial",
               n.trees = 150, 
               interaction.depth = 3,
               shrinkage = 0.01,
               cv.folds = 5,
               n.minobsinnode = 10,
               verbose = FALSE)
gbm_pred_probs <- predict(gbm_fit, newdata = test_dataset[,-15], n.trees = gbm.perf(gbm_fit, method = "cv"), type = "response")
gb_predictions <- colnames(gbm_pred_probs)[apply(gbm_pred_probs, 1, which.max)]
gb_confusion <- confusionMatrix(factor(gb_predictions), factor(test_dataset$Obesity))
print(gb_confusion)
precision <- conf_matrix$byClass["Precision"]
accuracy <- conf_matrix$overall["Accuracy"]
recall <- conf_matrix$byClass["Sensitivity"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
f1_score <- (2 * precision * recall) / (precision + recall)
print(paste("Gradient Boost Values"))
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))
stats_gb<- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

# Assuming "1" corresponds to the "Obese" class, and "0" corresponds to the "Non-Obese" class
gbm_pred_probs_class_1 <- gbm_pred_probs[, 2, "150"]  # Extract probabilities for class "1" (Obese)

# Create prediction object
gbm_pred <- prediction(gbm_pred_probs_class_1, test_dataset$Obesity)

# Create performance object
gbm_perf <- performance(gbm_pred, "tpr", "fpr")

# Plot ROC curve
plot(gbm_perf, main = "ROC Curve for Gradient Boosting Classifier", col = "purple", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "blue")


########## CART ##############################################

library(rpart)
library(class) # For additional classification functions
# install.packages("rpart") functions for building and visualizing CART

nrow(my_data) # missing values
my_data <- na.omit(my_data)
nrow(my_data) # check after missing values

# CART- Classification


# Train the CART model
cart_model <- rpart(Obesity ~ ., data = train_dataset, method = "class")

# Print the summary of the CART model
summary(cart_model)

# Make predictions on the testing set
predictions <- predict(cart_model, test_dataset, type = "class")

# Evaluate the performance of the CART model
confusion_matrix <- table(predictions, test_dataset$Obesity)
print(confusion_matrix)



# Calculate TP, TN, FP, and FN
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

# Calculate accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)

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
print(paste("CART Values"))
cat("CART Accuracy:", accuracy, "%\n")
cat("CART Sensitivity:", sensitivity, "%\n")
cat("CART Specificity:", specificity, "%\n")
cat("CART Precision:", precision, "%\n")
cat("CART Recall:", recall, "%\n")
cat("CART F1-score:", f1_score, "%\n")

# Visualize the decision tree
plot(cart_model, margin = 0.1)
text(cart_model, use.n = TRUE, cex = 0.6)

stats_cart<- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

# Get predicted probabilities for class 1
predicted_probabilities <- predict(cart_model, test_dataset, type = "prob")[, "1"]

# Create a prediction object
cart_pred <- prediction(predicted_probabilities, test_dataset$Obesity)

# Create performance object
cart_perf <- performance(cart_pred, "tpr", "fpr")

# Plot ROC curve
plot(cart_perf, main = "ROC Curve for CART Classifier", col = "blue", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, col = "red")


############# SVM ##############################

library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

svm <- svm(Obesity ~ ., data = train_dataset, kernel = "linear")

# Make predictions on the testing set
predictions <- predict(svm, newdata = test_dataset)

# Evaluate the performance of the SVM model
confusion_matrix <- table(predictions, test_dataset$Obesity) * 100
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
print(paste("SVM Values"))
cat("SVM Accuracy:", accuracy, "\n")
cat("SVM Precision:", precision, "\n")
cat("SVM Recall:", recall, "\n")
cat("SVM F1-score:", f1_score, "\n")
cat("SVM Sensitivity:", sensitivity, "\n")
cat("SVM Specificity:", specificity, "\n")
library(e1071)
library(ROCR)

# Train the SVM model with probability estimates
svm <- svm(Obesity ~ ., data = train_data, kernel = "linear", probability = TRUE)

# Make predictions on the testing set with probability estimates
predictions_prob <- predict(svm, newdata = test_data, probability = TRUE)

# Extract probabilities for the positive class
predictions_prob <- attr(predictions_prob, "probabilities")[, 2]

# Create prediction object
pred <- prediction(predictions_prob, test_data$Obesity)

# Create performance object
perf <- performance(pred, "tpr", "fpr")

# Plot ROC curve
plot(perf, main = "ROC Curve for SVM Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")


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
feature_names <- names(train_dataset)[!names(train_dataset) %in% c("Obesity")]

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
stats_svm<- c(accuracy, precision, recall, f1_score, sensitivity, specificity)

######### plots #################################

accuracy_data <- as.data.frame(t(data.frame(stats_knn, stats_lr, stats_rf, stats_ada, stats_naive, stats_gb, 
                            stats_cart, stats_svm)))
names(accuracy_data) <- c("Accuracy", "Precision", "Recall", "F1_score", "Sensitivity", "Specificity")
accuracy_data$Algorithm <- c("KNN", "Logistic Regression", "Random Forest", "Adaboost", "Naive Bayes",
                          "Gradient Boost", "CART", "SVM")

#accuracy_data <- as.data.frame(t(accuracy_data))

ggplot(accuracy_data, aes(x = Algorithm, y = Accuracy, color = Algorithm)) + geom_boxplot(coef = 5) + 
  ggtitle ("Accuracy Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))
color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")


ggplot(accuracy_data, aes(x = Algorithm, y = Accuracy, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("Accuracy Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(accuracy_data, aes(x = Algorithm, y = Precision, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("Precision Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accuracy_data, aes(x = Algorithm, y = Recall, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("Recall Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accuracy_data, aes(x = Algorithm, y = F1_score, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("F1_score Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accuracy_data, aes(x = Algorithm, y = Sensitivity, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("Sensitivity Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(accuracy_data, aes(x = Algorithm, y = Specificity, fill = Algorithm)) +
  geom_bar(stat = "identity") +
  ggtitle("Specificity Comparison of Different Algorithms") + 
  theme(plot.title = element_text(hjust = 0.5))
