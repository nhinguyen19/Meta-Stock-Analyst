install.packages("tseries")   
install.packages("forecast")
install.packages("dplyr")
install.packages("lubridate")


library(tseries)
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd("D:/2024-2025/HK2/PhanTichDuLieu/Project/R")

df <- read.csv("META.csv")

df$Date <- dmy(df$Date)

head(df$Date)

plot(df$Date, df$Close, type = "l", col = "blue",
     xlab = "Date", ylab = "Close Price", main = "META Closing Price Over Time")

#Kiem dinh ADF
adf.test(df$Close)

#Lay Sai phan bac 1
close_diff <- diff(df$Close, differences = 1)

#Plot de xem tinh dung
plot(df$Date[-1], close_diff, type = "l", col = "green",
     xlab = "Date", ylab = "Diff Close Price", main = "MSFT Diff Closing Price Over Time")

#Kiem dinh ADF
adf.test(close_diff)

#Plot ACF xac dinh q 
acf(close_diff, main = "ACF of Differenced Closed Price")

#Plot PACF xac dinh p
pacf(close_diff, main = "PACF of Differenced Closed Price")

#Xây dung mo hinh ARIMA (0,1,1)
model_arima<- arima(df$Close, order = c(0,1,1))

summary(model_arima)

#plot mô hinh auto ARIMA
n <- nrow(df['Close'])
train_size <- floor(0.9 * n)
test_size <- n - train_size

train <- df$Close[1:train_size]
test <- df$Close[(train_size + 1):n]

train_dates <- df$Date[1:train_size]
test_dates <- df$Date[(train_size + 1):n]

model_arima_manual <- arima(train, order = c(0,1,1))

forecast_result1_manual <- forecast(model_arima_manual, h = test_size)

# Dữ liệu thực tế (test)
actual_data <- data.frame(Date = test_dates, Close = test)

# Dữ liệu dự báo
forecast_data_manual <- data.frame(Date = test_dates,
                            Forecast = as.numeric(forecast_result1_manual$mean))
head(forecast_data_manual)

mae <- mean(abs(forecast_data_manual$Forecast - test))
mse <- mae <- mean((forecast_data_manual$Forecast - test)^2)
mape <- mean(abs((forecast_data_manual$Forecast - test)/test))*100

cat("MAE: ", mae)
cat("MSE:", mse)
cat("MAPE:",mape)

ggplot() +
  geom_line(data = data.frame(Date = train_dates, Close = train),
            aes(x = Date, y = Close, color = "Train")) +
  geom_line(data = actual_data,
            aes(x = Date, y = Close, color = "Actual")) +
  geom_line(data = forecast_data_manual,
            aes(x = Date, y = Forecast, color = "Forecast", linetype = "Forecast")) +
  scale_color_manual(values = c("Train" = "blue", "Actual" = "green", "Forecast" = "red")) +
  scale_linetype_manual(values = c("Train" = "solid", "Actual" = "solid", "Forecast" = "solid")) +
  labs(title = "ARIMA (1,1,1) Forecast vs Actual Data (Close Price)",
       x = "Date",
       y = "Close Price",
       color = "Series",
       linetype = "Series") +
  theme_minimal()


#Auto ARIMA
model_auto_arima<- auto.arima(df$Close)

summary(model_auto_arima)

#plot mô hinh auto ARIMA
df$Date <- as.Date(df$Date)

n <- nrow(df['Close'])
train_size <- floor(0.9 * n)
test_size <- n - train_size

train <- df$Close[1:train_size]
test <- df$Close[(train_size + 1):n]

train_dates <- df$Date[1:train_size]
test_dates <- df$Date[(train_size + 1):n]

model_auto_arima <- auto.arima(train)
forecast_result1 <- forecast(model_auto_arima, h = test_size)

# Dữ liệu thực tế (test)
actual_data <- data.frame(Date = test_dates, Close = test)

# Dữ liệu dự báo
forecast_data <- data.frame(Date = test_dates,
                            Forecast = as.numeric(forecast_result1$mean))

ggplot() +
  geom_line(data = data.frame(Date = train_dates, Close = train),
            aes(x = Date, y = Close, color = "Train")) +
  geom_line(data = actual_data,
            aes(x = Date, y = Close, color = "Actual")) +
  geom_line(data = forecast_data,
            aes(x = Date, y = Forecast, color = "Forecast", linetype = "Forecast")) +
  scale_color_manual(values = c("Train" = "blue", "Actual" = "green", "Forecast" = "red")) +
  scale_linetype_manual(values = c("Train" = "solid", "Actual" = "solid", "Forecast" = "dashed")) +
  labs(title = "ARIMA Forecast vs Actual Data (Close Price)",
       x = "Date",
       y = "Close Price",
       color = "Series",
       linetype = "Series") +
  theme_minimal()


