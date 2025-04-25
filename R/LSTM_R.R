
library(keras)
library(reticulate)
py_config()

data <- read.csv("META.csv")

features <- c('Close', 'High', 'Open', 'Low', 'Volume')

#Chuan hoa du lieu
data_scaled <- as.data.frame(scale(data[features]))

#Chia tap train, test
n <- nrow(data_scaled)

train_size <- floor(0.8*n)

train_data <- data_scaled[1:train_size,]
test_data <- data_scaled[(train_size + 1) : n,]

look_back <- 60

#Tap Train
X_train <- matrix(0, nrow = nrow(train_data) - look_back, ncol = look_back * ncol(train_data))
y_train <- numeric(nrow(train_data) - look_back)
  
for(i in 1:(nrow(train_data) - look_back)) {
  X_train[i,] <- as.vector(t(train_data[i:(i + look_back - 1), ]))  
  y_train[i] <- train_data[i + look_back, "Close"]  
}
  
X_train <- array_reshape(X_train, dim = c(nrow(X_train), look_back, ncol(train_data)))


#Tap test
X_test <- matrix(0, nrow = nrow(test_data) - look_back, ncol = look_back * ncol(test_data))
y_test <- numeric(length = nrow(test_data) - look_back)

for(i in 1:(nrow(test_data) - look_back)) {
  X_test[i,] <- as.vector(t(test_data[i:(i + look_back - 1), ]))  # Tạo mẫu từ các bước thời gian
  y_test[i] <- test_data[i + look_back, "Close"]  # Giá trị đóng cửa của bước tiếp theo
}

X_test <- array_reshape(X_test, dim = c(nrow(X_test), look_back, ncol(test_data)))


# Kiểm tra shape của X_train và X_test
dim(X_train)
dim(X_test)

# Xây dựng mô hình LSTM
input <- layer_input(shape = c(60, 5))

output <- input %>%
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dense(units = 1)

model <- keras_model_sequential(inputs = input, outputs = output)

model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(60, 5), return_sequences = FALSE) %>%
  layer_dense(units = 1)


# Tóm tắt mô hình
summary(model)
s
# Biên dịch mô hình
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Huấn luyện mô hình
history <- model %>% fit(X_train, y_train, epochs = 100, batch_size = 32, validation_data = list(X_test, y_test))

# Đánh giá mô hình trên tập kiểm tra
score <- model %>% evaluate(X_test, y_test)
print(paste("Test loss:", score))

# Dự đoán giá trị trên dữ liệu kiểm tra
predictions <- model %>% predict(X_test)

# Vẽ kết quả dự đoán so với giá trị thực tế
plot(y_test, type = "l", col = "blue", lwd = 2, ylim = c(min(c(y_test, predictions)), max(c(y_test, predictions))))
lines(predictions, col = "red", lwd = 2)



# Chuẩn bị dữ liệu huấn luyện
time_steps <- 5
train_data_prepared <- prepare_data(train_data, time_steps)

X_train <- train_data_prepared$X
y_train <- train_data_prepared$y


# Xây dựng mô hình LSTM
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = FALSE, input_shape = c(time_steps, 1)) %>%
  layer_dense(units = 1)

# Tóm tắt mô hình
summary(model)

# Biên dịch mô hình
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Huấn luyện mô hình
model %>% fit(X_train, y_train, epochs = 10, batch_size = 32)

# Dự báo trên dữ liệu huấn luyện
predictions <- model %>% predict(X_train)

# Hiển thị các dự báo
predictions

# Chuẩn bị dữ liệu kiểm tra
test_data_prepared <- prepare_data(test_data, time_steps)

X_test <- test_data_prepared$X
y_test <- test_data_prepared$y

# Dự báo trên dữ liệu kiểm tra
test_predictions <- model %>% predict(X_test)

# Tính MSE (Mean Squared Error)
mse <- mean((test_predictions - y_test)^2)
cat("MSE on Test Data:", mse, "\n")


