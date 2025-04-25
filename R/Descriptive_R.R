setwd("D:/2024-2025/HK2/PhanTichDuLieu/Project")

library(e1071)
library(ggplot2)

df <- read.csv("META.csv")

summary(df)

#Phuong sai, do lech chuan

variance_close <- var(df$Close)

print(variance_close)

sd_close <- sd(df$Close)

print(sd_close)

variance_volume <- var(df$Volume)

print(variance_volume)

sd_volume <- sd(df$Volume)

print(sd_volume)

#Skewness va Kurtosis

skewness_close <- skewness(df$Close)

kurtosis_close <- kurtosis(df$Close)

cat("Skewness Close: " ,{skewness_close})
cat("Kurtosis Close: ", kurtosis_close)


skewness_volume <- skewness(df$Volume)

kurtosis_volume <- kurtosis(df$Volume)

cat("Skewness Volume: " ,skewness_volume)
cat("Kurtosis Volume: ", kurtosis_volume)




