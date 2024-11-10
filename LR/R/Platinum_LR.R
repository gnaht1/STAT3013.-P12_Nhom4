# Cài đặt gói nếu chưa có
install.packages("caTools")
install.packages("readxl")
library(caTools)
library(readxl)

# Đọc dữ liệu từ file Excel
df <- read_excel("E:\\Code\\STAT3013.-P12_Nhom4\\Dataset\\Platinum_data_filtered.xlsx")

# Thêm cột index để sử dụng làm biến đầu vào
df$index <- 1:nrow(df)

# Đặt seed để kết quả có thể tái hiện
set.seed(123)

# Chia dữ liệu theo tỷ lệ 7:2:1
split_7_2_1 <- sample.split(df$close, SplitRatio = 0.7)
train_7_2_1 <- subset(df, split_7_2_1 == TRUE)
remaining_7_2_1 <- subset(df, split_7_2_1 == FALSE)

split_2 <- sample.split(remaining_7_2_1$close, SplitRatio = 2/3)
test_7_2_1 <- subset(remaining_7_2_1, split_2 == TRUE)
validation_7_2_1 <- subset(remaining_7_2_1, split_2 == FALSE)

# Chia dữ liệu theo tỷ lệ 6:3:1
split_6_3_1 <- sample.split(df$close, SplitRatio = 0.6)
train_6_3_1 <- subset(df, split_6_3_1 == TRUE)
remaining_6_3_1 <- subset(df, split_6_3_1 == FALSE)

split_3 <- sample.split(remaining_6_3_1$close, SplitRatio = 3/4)
test_6_3_1 <- subset(remaining_6_3_1, split_3 == TRUE)
validation_6_3_1 <- subset(remaining_6_3_1, split_3 == FALSE)

# Chia dữ liệu theo tỷ lệ 5:3:2
split_5_3_2 <- sample.split(df$close, SplitRatio = 0.5)
train_5_3_2 <- subset(df, split_5_3_2 == TRUE)
remaining_5_3_2 <- subset(df, split_5_3_2 == FALSE)

split_3 <- sample.split(remaining_5_3_2$close, SplitRatio = 3/5)
test_5_3_2 <- subset(remaining_5_3_2, split_3 == TRUE)
validation_5_3_2 <- subset(remaining_5_3_2, split_3 == FALSE)

# Tạo hàm để huấn luyện mô hình, in summary và dự đoán giá bạch kim 30 ngày tiếp theo
predict_next_30_days <- function(train_data, original_data) {
  # Huấn luyện mô hình hồi quy tuyến tính sử dụng cột index
  model <- lm(close ~ index, data = train_data)
  
  # In thông tin chi tiết về mô hình
  print(summary(model))
  
  # Tạo dữ liệu dự đoán cho 30 ngày tiếp theo
  last_index <- max(train_data$index)
  next_30_days <- data.frame(index = (last_index + 1):(last_index + 30))
  
  # Dự đoán giá bạch kim
  next_30_days$predicted_close <- predict(model, newdata = next_30_days)
  
  # Trả về kết quả dự đoán
  return(next_30_days)
}

# Dự đoán và in summary cho các tỷ lệ chia dữ liệu
print("Tóm tắt mô hình cho tỷ lệ 7:2:1:")
next_30_days_7_2_1 <- predict_next_30_days(train_7_2_1, df)

print("Tóm tắt mô hình cho tỷ lệ 6:3:1:")
next_30_days_6_3_1 <- predict_next_30_days(train_6_3_1, df)

print("Tóm tắt mô hình cho tỷ lệ 5:3:2:")
next_30_days_5_3_2 <- predict_next_30_days(train_5_3_2, df)

# Hiển thị kết quả dự đoán
print("Dự đoán giá bạch kim 30 ngày tiếp theo cho tỷ lệ 7:2:1:")
print(next_30_days_7_2_1)

print("Dự đoán giá bạch kim 30 ngày tiếp theo cho tỷ lệ 6:3:1:")
print(next_30_days_6_3_1)

print("Dự đoán giá bạch kim 30 ngày tiếp theo cho tỷ lệ 5:3:2:")
print(next_30_days_5_3_2)

# Vẽ đồ thị cho từng tập dự đoán
library(ggplot2)

plot_predictions <- function(original_data, next_30_days, title) {
  ggplot() +
    geom_line(data = original_data, aes(x = index, y = close), color = "blue") +
    geom_line(data = next_30_days, aes(x = index, y = predicted_close), color = "red") +
    labs(title = title, x = "Index", y = "Platinum")
}

# Vẽ đồ thị cho từng tỷ lệ chia
plot_predictions(df, next_30_days_7_2_1, "Platinum price for the next 30 days (Scale 7:2:1)")
plot_predictions(df, next_30_days_6_3_1, "Platinum price for the next 30 days (Scale 6:3:1)")
plot_predictions(df, next_30_days_5_3_2, "Platinum price for the next 30 days (Scale 5:3:2)")
