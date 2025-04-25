# Install necessary packages if not already installed
install.packages(c("data.table", "ggplot2", "caret", "randomForest", "pROC", "superml"))

# Load libraries
library(data.table)
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)
library(superml)

# Read the dataset
df <- fread("C:/Users/nghil/Downloads/META_stocks_with_Level.csv")

# Convert 'Date' column to Date format
df[, Date := as.Date(Date)]

# Define feature columns
features <- c("Open", "High", "Low", "Close", "Adj Close", "Volume")

# Remove rows with missing values in specified columns
df_clean <- df[complete.cases(df[, c(features, "Level"), with = FALSE])]

# Calculate counts for each Level
level_counts <- table(df_clean$Level)
level_df <- as.data.frame(level_counts)
colnames(level_df) <- c("Level", "Count")

# Create pie chart
ggplot(level_df, aes(x = "", y = Count, fill = Level)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Tỉ lệ Level: Increase vs Decrease") +
  theme_void()

# Initialize LabelEncoder
lbl <- LabelEncoder$new()

# Fit and transform 'Level' column
df_clean$Level_encoded <- lbl$fit_transform(df_clean$Level)

set.seed(42)
train_indices <- createDataPartition(df_clean$Level_encoded, p = 0.8, list = FALSE)
train_data <- df_clean[train_indices]
test_data <- df_clean[-train_indices]

# Train the model
rf_model <- randomForest(x = train_data[, ..features], 
                         y = as.factor(train_data$Level_encoded), 
                         ntree = 100, 
                         importance = TRUE)

# Predictions
train_pred <- predict(rf_model, train_data[, ..features])
test_pred <- predict(rf_model, test_data[, ..features])

# Confusion Matrices
confusionMatrix(train_pred, as.factor(train_data$Level_encoded))
confusionMatrix(test_pred, as.factor(test_data$Level_encoded))

# Accuracy
train_accuracy <- mean(train_pred == train_data$Level_encoded)
test_accuracy <- mean(test_pred == test_data$Level_encoded)

# MAE and RMSE
train_mae <- mean(abs(as.numeric(train_pred) - train_data$Level_encoded))
test_mae <- mean(abs(as.numeric(test_pred) - test_data$Level_encoded))

train_rmse <- sqrt(mean((as.numeric(train_pred) - train_data$Level_encoded)^2))
test_rmse <- sqrt(mean((as.numeric(test_pred) - test_data$Level_encoded)^2))

# Predict probabilities
train_prob <- predict(rf_model, train_data[, ..features], type = "prob")[,2]
test_prob <- predict(rf_model, test_data[, ..features], type = "prob")[,2]

# ROC and AUC for training set
roc_train <- roc(train_data$Level_encoded, train_prob)
auc_train <- auc(roc_train)

# ROC and AUC for testing set
roc_test <- roc(test_data$Level_encoded, test_prob)
auc_test <- auc(roc_test)

# Plot ROC curves
plot(roc_train, col = "blue", main = "ROC Curve")
lines(roc_test, col = "red")
legend("bottomright", legend = c(paste("Train AUC =", round(auc_train, 2)),
                                 paste("Test AUC =", round(auc_test, 2))),
       col = c("blue", "red"), lwd = 2)

# Calculate mean of last 60 rows
mean_values <- colMeans(tail(df_clean[, ..features], 60))

# Create future data
future_data <- as.data.frame(matrix(rep(mean_values, 30), nrow = 30, byrow = TRUE))
colnames(future_data) <- features

# Predict future levels
future_pred <- predict(rf_model, future_data)
future_labels <- lbl$inverse_transform(future_pred)

# Generate future dates
last_date <- max(df_clean$Date)
future_dates <- seq.Date(from = last_date + 1, by = "day", length.out = 30)

# Create forecast dataframe
forecast_df <- data.frame(Date = future_dates, Predicted_Level = future_labels)

# Convert 'Increase' to 1 and 'Decrease' to 0
forecast_df$Level_numeric <- ifelse(forecast_df$Predicted_Level == "Increase", 1, 0)

# Plot
ggplot(forecast_df, aes(x = Date, y = Level_numeric)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen") +
  labs(title = "Dự đoán xu hướng Level trong 30 ngày tới",
       x = "Ngày",
       y = "Level (1 = Increase, 0 = Decrease)") +
  theme_minimal()


