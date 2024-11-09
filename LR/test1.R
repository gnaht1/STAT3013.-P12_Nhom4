# Cài đặt gói nếu chưa có
install.packages("readxl")

# Đọc tệp Excel bằng readxl
library(readxl)
df <- read_excel("E:/Code/STAT3013.-P12_Nhom4/Dataset/Gold_data_filtered - Copy.xlsx")

df$date_numeric <- as.numeric(as.Date(df$date, format = "%Y-%m-%d"))

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(close ~ date_numeric, data = df)

# Kiểm tra tóm tắt kết quả
summary(model)

#Tao data cho 30 ngay tiep theo
last_date <- max(df$date_numeric)
next_30_days <- data.frame(date_numeric = (last_date + 1):(last_date + 30))

predictions <- predict(model, newdata = next_30_days)

# Thêm cột dự đoán vào dataframe
next_30_days$predicted_close <- predictions

#Kiem tra du doan
print(next_30_days)

#Ve do thi
library(ggplot2)

# Vẽ dữ liệu thực tế và dự đoán
ggplot() +
  geom_line(data = df, aes(x = date_numeric, y = close), color = "blue") +
  geom_line(data = next_30_days, aes(x = date_numeric, y = predicted_close), color = "red") +
  labs(title = "Dự đoán giá vàng 30 ngày tiếp theo", x = "Ngày (numeric)", y = "Giá vàng")



