data <- read.csv("C:/Users/DELL/OneDrive/Tài liệu/META.csv")

# Cài thư viện nếu chưa có
if (!require("Metrics")) install.packages("Metrics", dependencies=TRUE)
library(Metrics)

# Mô hình 1: có Volume
model_with_volume <- lm(Close ~ Open + High + Low + Volume, data = data)
summary_with_volume <- summary(model_with_volume)

# Mô hình 2: không có Volume
model_without_volume <- lm(Close ~ Open + High + Low, data = data)
summary_without_volume <- summary(model_without_volume)

# Hàm tính MAE, MSE, RMSE
get_errors <- function(model, data) {
  preds <- predict(model, newdata = data)
  actuals <- data$Close
  mae_val <- mae(actuals, preds)
  mse_val <- mse(actuals, preds)
  rmse_val <- rmse(actuals, preds)
  return(c(MAE = mae_val, MSE = mse_val, RMSE = rmse_val))
}

# Hàm lấy thông số từ model summary
get_model_stats <- function(summary_model) {
  list(
    Multiple_R = sqrt(summary_model$r.squared),
    R_squared = summary_model$r.squared,
    Adj_R_squared = summary_model$adj.r.squared,
    Significance_F = pf(summary_model$fstatistic[1],
                        summary_model$fstatistic[2],
                        summary_model$fstatistic[3],
                        lower.tail = FALSE),
    Coefficients = summary_model$coefficients
  )
}

# Kết quả
cat("=== Mô hình CÓ Volume ===\n")
print(get_model_stats(summary_with_volume))
print(get_errors(model_with_volume, data))

cat("\n=== Mô hình KHÔNG Volume ===\n")
print(get_model_stats(summary_without_volume))
print(get_errors(model_without_volume, data))
# vẽ biểu đồ pairlot 
# Cài đặt 
install.packages("GGally")   
library(GGally)

# Lọc ra các biến số để tránh lỗi (nếu có biến kiểu ký tự hoặc ngày tháng)
data_numeric <- data[sapply(data, is.numeric)]

# Vẽ biểu đồ pairplot
ggpairs(data_numeric,
        title = "Biểu đồ tương quan giữa các biến số")

ggpairs(data_numeric,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("barDiag", fill = "lightblue")),
        title = "Biểu đồ tương quan giữa các biến số")
