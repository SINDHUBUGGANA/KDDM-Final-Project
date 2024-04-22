library(ggplot2)
library(dplyr)
library(stringr)
rm(list = ls())

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
