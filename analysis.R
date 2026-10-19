
# Week 1: Data Cleaning and Preliminary Analysis with R

library(dplyr)
library(ggplot2)
library(tidyr)

# Sample Representative Titanic Dataset (Direct In-Memory)
titanic_data <- data.frame(
  PassengerId = 1:20,
  Survived = c(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1),
  Pclass = c(3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 2, 3, 3),
  Name = paste("Passenger", 1:20),
  Sex = c("male", "female", "female", "female", "male", "male", "male", "male", 
          "female", "female", "female", "female", "male", "male", "female", 
          "female", "male", "male", "female", "female"),
  Age = c(22, 38, 26, 35, 35, NA, 54, 2, 27, 14, 4, 58, 20, 39, 14, 55, 2, NA, 31, NA),
  SibSp = c(1, 1, 0, 1, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, 0, 1, 0),
  Parch = c(0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, 0, 0, 0),
  Fare = c(7.25, 71.28, 7.92, 53.1, 8.05, 8.45, 51.86, 21.07, 11.13, 30.07, 
           16.7, 26.55, 8.05, 31.27, 7.85, 16.0, 29.12, 13.0, 18.0, 110.0),
  Embarked = c("S", "C", "S", "S", "S", "Q", "S", "S", "S", "C", "S", "S", "S", "S", "S", "S", "Q", "S", "S", ""),
  Cabin = c(NA, "C85", NA, "C123", NA, NA, "E46", NA, NA, NA, "G6", "C103", NA, NA, NA, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

cat("--- Initial Data Structure ---\n")
str(titanic_data)

# 1. Missing Value Imputation
median_age <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Age[is.na(titanic_data$Age)] <- median_age
titanic_data$Embarked[titanic_data$Embarked == "" | is.na(titanic_data$Embarked)] <- "S"
titanic_data <- titanic_data %>% select(-Cabin)

# 2. Outlier Capping (IQR Method)
Q1 <- quantile(titanic_data$Fare, 0.25)
Q3 <- quantile(titanic_data$Fare, 0.75)
IQR_fare <- Q3 - Q1
upper_threshold <- Q3 + 1.5 * IQR_fare
titanic_data$Fare_Capped <- ifelse(titanic_data$Fare > upper_threshold, upper_threshold, titanic_data$Fare)

# 3. Factor Conversion
titanic_data$Survived <- factor(titanic_data$Survived, levels = c(0, 1), labels = c("No", "Yes"))
titanic_data$Pclass <- factor(titanic_data$Pclass, levels = c(1, 2, 3), labels = c("1st", "2nd", "3rd"))
titanic_data$Sex <- factor(titanic_data$Sex)
titanic_data$Embarked <- factor(titanic_data$Embarked)

cat("\n--- Cleaned Data Summary ---\n")
summary(titanic_data[, c("Age", "Fare_Capped", "Survived", "Sex", "Pclass")])

# 4. Survival Rates
cat("\n--- Survival by Gender (%) ---\n")
print(round(prop.table(table(titanic_data$Sex, titanic_data$Survived), margin = 1) * 100, 2))

# 5. Visualizations
p1 <- ggplot(titanic_data, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Proportion by Gender", y = "Proportion", x = "Gender") +
  theme_minimal()

p2 <- ggplot(titanic_data, aes(x = Age)) +
  geom_histogram(binwidth = 10, fill = "#1F618D", color = "white") +
  labs(title = "Distribution of Age (Post-Imputation)", x = "Age", y = "Count") +
  theme_minimal()

print(p1)
print(p2)
