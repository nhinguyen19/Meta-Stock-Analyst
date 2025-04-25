setwd("D:/2024-2025/HK2/PhanTichDuLieu/Project/R")

library(keras3)
library(caret)

data <- read.csv("META.csv")

# Chuẩn hóa dữ liệu sử dụng scale()
features <- c('Open', 'High', 'Low', 'Volume', 'Close')

min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_close <- min(data$Close)
max_close <- max(data$Close)

#scaled_data1 <- as.data.frame(scale(data[features], center = TRUE, scale = TRUE))
scaled_data1 <- as.data.frame(lapply(data[features], min_max_scale))
# Kiểm tra kết quả
head(scaled_data1)

#do lech chuan close
standard <- sd(data$Close)

mean_close <- mean(data$Close)

n <- nrow(scaled_data1)

# Split data into training and testing
train_size <- floor(n * 0.8)
train_data <- scaled_data1[1:train_size, ]
test_data <- scaled_data1[(train_size + 1):n, ]

dim(train_data)
dim(test_data)

# Tạo hàm tạo dữ liệu cho LSTM với 60 ngày (lookback)
create_dataset_60 <- function(data, lookback) {
  X <- list()
  y <- vector()
  
  for (i in (lookback + 1):nrow(data)) {
    X[[length(X) + 1]] <- as.matrix(data[(i - lookback):(i - 1), ])  # Dữ liệu X
    y <- c(y, data[i, 5])  # Dữ liệu y (column 5 là 'Close' tương ứng với y)
  }
  
  X <- array(unlist(X), dim = c(length(X), lookback, ncol(data)))
  
  return(list(X = X, y = y))
}

lookback <- 60

train_result <- create_dataset_60(train_data, lookback)
X_train1 <- train_result$X
y_train1 <- train_result$y

test_result <- create_dataset_60(test_data, lookback)
X_test1 <- test_result$X
y_test1 <- test_result$y

# In ra kích thước của X_train1 và X_test1
cat("X_train1 shape: ", dim(X_train1), "\n")
cat("X_test1 shape: ", dim(X_test1), "\n")

model <- keras_model_sequential() %>%
  layer_lstm(units = 64, return_sequences = FALSE, input_shape = c(60, ncol(features))) %>%
  layer_dense(units = 1)

# Compile model
model %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error'
)

# Train model
model %>% fit(X_train1, y_train1, epochs = 80, batch_size = 32)

# Dự đoán giá trị từ mô hình
predicted <- predict(model, X_test1)

# Khởi tạo ma trận để lưu các giá trị đầy đủ
close_index <- 5  # Chỉ số của cột 'Close' (tương ứng với cột thứ 5 trong dữ liệu)

# Tạo các ma trận rỗng có kích thước phù hợp
predicted_full <- matrix(0, nrow = length(predicted), ncol = length(features))
real_full <- matrix(0, nrow = length(y_test1), ncol = length(features))

# Lấp đầy cột 'Close' với các giá trị dự đoán và thực tế
predicted_full[, close_index] <- predicted[, 1]
real_full[, close_index] <- y_test1

#Ham dao nguoc
inverse_min_max <- function(x, min_value, max_value) {
  x * (max_value - min_value) + min_value
}


# Lấy cột 'Close' từ predicted_full
predicted_close <- predicted_full[, close_index]

predicted_prices_real <- abs(inverse_min_max(predicted_close, min_close, max_close))

# Đảo ngược chuẩn hóa cho dữ liệu thực tế
actual_close <- real_full[,close_index]

actual_test_prices <- abs(inverse_min_max(actual_close, min_close, max_close))

head(actual_test_prices)

# In kết quả
cat("Predicted Prices: ", head(predicted_prices_real), "\n")
cat("Actual Test Prices: ", head(actual_test_prices), "\n")

data_plot <- data.frame(
  Time = 1:length(predicted_prices_real),
  Predicted = predicted_prices_real,
  Actual = actual_test_prices
)

library(ggplot2)
# Vẽ đồ thị
ggplot(data_plot, aes(x = Time)) +
  geom_line(aes(y = Predicted, color = "Dự đoán")) + 
  geom_line(aes(y = Actual, color = "Thực tế")) + 
  labs(title = "LSTM dự đoán giá Close (đa biến)", 
       x = "Thời gian", 
       y = "Giá Close") + 
  scale_color_manual(name = "Chú thích", values = c("Dự đoán" = "blue", "Thực tế" = "red")) +
  theme_minimal()

#Tinh MAE
mae <- mean(abs(predicted_prices_real - actual_test_prices))
cat("MAE: ", mae)

mse <- mean((predicted_prices_real - actual_test_prices)^2)
cat("MSE: ", mse)

mape <- (mean(abs(predicted_prices_real - actual_test_prices))/ actual_test_prices)*100
cat("MSE: ", mse, "%")
