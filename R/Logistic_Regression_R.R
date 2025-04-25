setwd("D:/Users/HP/Documents/Năm ba/2. Phân tích dữ liệu kinh doanh/Đồ án cuối kỳ")

# Import thư viện
library(ggplot2)
library(caret)
library(Metrics)
library(dplyr)
library(caTools)
library(glmnet)

#Đọc dữ liệu
df <- read.csv('META.csv')
df <- df[, !(names(df) %in% "Year.Quarter")]
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")

# Kiểm tra dữ liệu
head(df)
str(df)
summary((df))

#Trực quan hóa cột Close theo theo Date
ggplot(df, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +
  labs(title = "Meta Stock Closing Prices",
       x = "Date", y = "Close Price") +
  theme_minimal()

#Chuẩn bị dữ liệu huấn luyện
data_train <- df[1:2565, ]
data_test <- df[2566:nrow(df), ]

X_train <- data_train[, !(names(data_train) %in% c("Close"))]
y_train <- data_train$Close

X_test <- data_test[, !(names(data_test) %in% c("Close"))]
y_test <- data_test$Close

# Huấn luyện mô hình Linear Regression
model <- train(X_train, y_train, method = "lm")

# Dự đoán
y_pred <- predict(model, X_test)

# Đánh giá mô hình
mae <- mae(y_test, y_pred)
mse <- mse(y_test, y_pred)
rmse <- rmse(y_test, y_pred)
r2 <- R2(y_pred, y_test)
mean_absolute_percentage_error <- function(y_true, y_pred) {
  mean(abs((y_true - y_pred) / y_true)) * 100
}
mape <- mean_absolute_percentage_error(y_test, y_pred)

# In kết quả đánh giá
cat(paste(
  "Kết quả đánh giá mô hình huấn luyện:\n",
  "MAE  :", round(mae, 4), "\n",
  "MSE  :", round(mse, 4), "\n",
  "RMSE :", round(rmse, 4), "\n",
  "MAPE :", round(mape, 2), "%\n",
  "R²   :", round(r2, 4), "\n"))

# Vẽ biểu đồ
data_test$Predictions <- y_pred

ggplot() +
  geom_line(data = data_train, aes(x = Date, y = Close, color = "Train")) +
  geom_line(data = data_test, aes(x = Date, y = Close, color = "Actual")) +
  geom_line(data = data_test, aes(x = Date, y = Predictions, color = "Predictions")) +
  scale_color_manual(values = c("Train" = "blue", "Actual" = "orange", "Predictions" = "green")) +
  labs(title = "Dự đoán giá Close bằng Linear Regression",
       x = "Ngày", y = "Giá Close", color = "Chú thích:") +
  theme_minimal() +
  theme(legend.position = "bottom")



#Huấn luyện mô hình hồi quy Logistic
# Tạo cột Target
n <- 30
df <- df %>%
  mutate(Target = as.integer(Close < lead(Close, n)))%>%
  na.omit()

head(df)
str(df)
sapply(df, function(x) sum(is.na(x)))

# Hồi quy logistic sử dụng glm
logit_model <- glm(Target ~ Open + High + Low + Close + Volume,
                   data = df, family = "binomial")
summary(logit_model)

# Lấy dữ liệu huấn luyện và kiểm tra
data_train <- df[1:2565, ]
data_test <- df[2566:nrow(df), ]

X_train <- data_train %>% select(Open, High, Low, Close, Volume)
y_train <- data_train$Target

X_test <- data_test %>% select(Open, High, Low, Close, Volume)
y_test <- data_test$Target

# Chuẩn hóa dữ liệu
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, X_train)
X_test_scaled <- predict(preproc, X_test)

# Huấn luyện mô hình logistic
X_train_matrix <- as.matrix(X_train_scaled)
X_test_matrix <- as.matrix(X_test_scaled)

logit_glmnet <- glmnet(X_train_matrix, y_train, family = "binomial", lambda = 0.015, control=1000)
y_pred_prob <- predict(logit_glmnet, newx = X_test_matrix, type = "response")
y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)

# Đánh giá mô hình
conf_mat <- confusionMatrix(factor(y_pred), factor(y_test))
print(conf_mat)

# Hiển thị Accuracy và Classification Report
cat("Accuracy:", round(conf_mat$overall['Accuracy'], 4), "\n")
cat("Classification Report:\n")
print(conf_mat$byClass)
