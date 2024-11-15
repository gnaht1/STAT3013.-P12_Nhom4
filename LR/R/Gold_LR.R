# Import các thư viện cần thiết
# Import các thư viện cần thiết
library(readxl)
library(caret)
library(ggplot2)
library(scales)

# Tải dữ liệu từ file Excel (chỉnh sửa đường dẫn nếu cần)
df <- read_excel("E:/Code/STAT3013.-P12_Nhom4/Dataset/Gold_data_filtered.xlsx")

# Thêm cột 'index' để sử dụng làm biến độc lập
df$index <- 1:nrow(df)

# Chuyển cột 'index' thành một data frame để chuẩn hóa
index_df <- data.frame(index = df$index)

# Chuẩn hóa cột 'index' để cải thiện hiệu suất mô hình dựa trên tập huấn luyện
scaler <- preProcess(index_df, method = c("center", "scale"))
df$index_normalized <- predict(scaler, index_df)$index

# Phân chia dữ liệu thành tập huấn luyện, kiểm tra và xác thực (70:20:10)
train_size <- floor(0.7 * nrow(df))
test_size <- floor(0.2 * nrow(df))
val_size <- nrow(df) - train_size - test_size

train_data <- df[1:train_size, ]
test_data <- df[(train_size + 1):(train_size + test_size), ]
val_data <- df[(train_size + test_size + 1):nrow(df), ]

# Chuẩn bị dữ liệu cho huấn luyện mô hình
x_train <- train_data$index_normalized
y_train <- train_data$close

# Huấn luyện mô hình hồi quy tuyến tính
model <- lm(y_train ~ x_train)

# Dự đoán trên tập kiểm tra
x_test <- test_data$index_normalized
y_test <- test_data$close
y_pred_test <- predict(model, newdata = data.frame(x_train = x_test))

# Đánh giá lỗi trên tập kiểm tra (MAE, RMSE, MAPE)
mae_test <- mean(abs(y_test - y_pred_test))
rmse_test <- sqrt(mean((y_test - y_pred_test)^2))
mape_test <- mean(abs((y_test - y_pred_test) / y_test)) * 100

# Kết quả lỗi trên tập kiểm tra
cat("MAE (Test):", mae_test, "\n")
cat("RMSE (Test):", rmse_test, "\n")
cat("MAPE (Test):", mape_test, "\n")

# Dự đoán trên tập xác thực
x_val <- val_data$index_normalized
y_val <- val_data$close
y_pred_val <- predict(model, newdata = data.frame(x_train = x_val))

# Đánh giá lỗi trên tập xác thực (MAE, RMSE, MAPE)
mae_val <- mean(abs(y_val - y_pred_val))
rmse_val <- sqrt(mean((y_val - y_pred_val)^2))
mape_val <- mean(abs((y_val - y_pred_val) / y_val)) * 100

# Kết quả lỗi trên tập xác thực
cat("MAE (Validation):", mae_val, "\n")
cat("RMSE (Validation):", rmse_val, "\n")
cat("MAPE (Validation):", mape_val, "\n")

# Dự đoán cho 30 ngày tiếp theo
last_index <- max(df$index)
new_indices <- (last_index + 1):(last_index + 30)
new_indices_df <- data.frame(index = new_indices)
new_indices_normalized <- predict(scaler, new_indices_df)$index
future_predictions <- predict(model, newdata = data.frame(x_train = new_indices_normalized))

# Tạo data frame cho từng phần dữ liệu
train_data_plot <- data.frame(Index = train_data$index, Price = y_train, Type = "Train")
test_data_plot <- data.frame(Index = test_data$index, Price = y_test, Type = "Test")
validate_data_plot <- data.frame(Index = val_data$index, Price = y_val, Type = "Validate")
predictions_test_plot <- data.frame(Index = test_data$index, Price = y_pred_test, Type = "Predictions (Test)")
predictions_val_plot <- data.frame(Index = val_data$index, Price = y_pred_val, Type = "Predictions (Validate)")
next_30_days_plot <- data.frame(Index = new_indices, Price = future_predictions, Type = "Next 30 Days")

# Gộp tất cả vào một data frame cho ggplot
plot_data <- rbind(train_data_plot, test_data_plot, validate_data_plot, 
                   predictions_test_plot, predictions_val_plot, next_30_days_plot)

# Vẽ biểu đồ
ggplot(plot_data, aes(x = Index, y = Price, color = Type)) +
  geom_line() +
  labs(title = "Gold Price Predictions: Train, Test, Validate, and Next 30 Days",
       x = "Index", y = "Gold Price") +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "green", 
                                "Validate" = "purple", 
                                "Predictions (Test)" = "orange",
                                "Predictions (Validate)" = "pink",
                                "Next 30 Days" = "red")) +
  theme_minimal()

# Tạo data frame cho dự đoán 30 ngày tiếp theo và in ra kết quả
future_data <- data.frame(Day = new_indices, Predicted_Price = future_predictions)
print("Dự đoán giá vàng cho 30 ngày tiếp theo:")
print(future_data)



  # Phân chia dữ liệu thành tập huấn luyện, kiểm tra và xác thực (60:30:10)
train_size <- floor(0.6 * nrow(df))
test_size <- floor(0.3 * nrow(df))
val_size <- nrow(df) - train_size - test_size

train_data <- df[1:train_size, ]
test_data <- df[(train_size + 1):(train_size + test_size), ]
val_data <- df[(train_size + test_size + 1):nrow(df), ]

# Chuẩn bị dữ liệu cho huấn luyện mô hình
x_train <- train_data$index_normalized
y_train <- train_data$close

# Huấn luyện mô hình hồi quy tuyến tính
model <- lm(y_train ~ x_train)

# Dự đoán trên tập kiểm tra
x_test <- test_data$index_normalized
y_test <- test_data$close
y_pred_test <- predict(model, newdata = data.frame(x_train = x_test))

# Đánh giá lỗi trên tập kiểm tra (MAE, RMSE, MAPE)
mae_test <- mean(abs(y_test - y_pred_test))
rmse_test <- sqrt(mean((y_test - y_pred_test)^2))
mape_test <- mean(abs((y_test - y_pred_test) / y_test)) * 100

# Kết quả lỗi trên tập kiểm tra
cat("MAE (Test):", mae_test, "\n")
cat("RMSE (Test):", rmse_test, "\n")
cat("MAPE (Test):", mape_test, "\n")

# Dự đoán trên tập xác thực
x_val <- val_data$index_normalized
y_val <- val_data$close
y_pred_val <- predict(model, newdata = data.frame(x_train = x_val))

# Đánh giá lỗi trên tập xác thực (MAE, RMSE, MAPE)
mae_val <- mean(abs(y_val - y_pred_val))
rmse_val <- sqrt(mean((y_val - y_pred_val)^2))
mape_val <- mean(abs((y_val - y_pred_val) / y_val)) * 100

# Kết quả lỗi trên tập xác thực
cat("MAE (Validation):", mae_val, "\n")
cat("RMSE (Validation):", rmse_val, "\n")
cat("MAPE (Validation):", mape_val, "\n")

# Dự đoán cho 30 ngày tiếp theo
last_index <- max(df$index)
new_indices <- (last_index + 1):(last_index + 30)
new_indices_df <- data.frame(index = new_indices)
new_indices_normalized <- predict(scaler, new_indices_df)$index
future_predictions <- predict(model, newdata = data.frame(x_train = new_indices_normalized))

# Tạo data frame cho từng phần dữ liệu
train_data_plot <- data.frame(Index = train_data$index, Price = y_train, Type = "Train")
test_data_plot <- data.frame(Index = test_data$index, Price = y_test, Type = "Test")
validate_data_plot <- data.frame(Index = val_data$index, Price = y_val, Type = "Validate")
predictions_test_plot <- data.frame(Index = test_data$index, Price = y_pred_test, Type = "Predictions (Test)")
predictions_val_plot <- data.frame(Index = val_data$index, Price = y_pred_val, Type = "Predictions (Validate)")
next_30_days_plot <- data.frame(Index = new_indices, Price = future_predictions, Type = "Next 30 Days")

# Gộp tất cả vào một data frame cho ggplot
plot_data <- rbind(train_data_plot, test_data_plot, validate_data_plot, 
                   predictions_test_plot, predictions_val_plot, next_30_days_plot)

# Vẽ biểu đồ
ggplot(plot_data, aes(x = Index, y = Price, color = Type)) +
  geom_line() +
  labs(title = "Gold Price Predictions: Train, Test, Validate, and Next 30 Days",
       x = "Index", y = "Gold Price") +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "green", 
                                "Validate" = "purple", 
                                "Predictions (Test)" = "orange",
                                "Predictions (Validate)" = "pink",
                                "Next 30 Days" = "red")) +
  theme_minimal()

# Tạo data frame cho dự đoán 30 ngày tiếp theo và in ra kết quả
future_data <- data.frame(Day = new_indices, Predicted_Price = future_predictions)
print("Dự đoán giá vàng cho 30 ngày tiếp theo:")
print(future_data)


# Phân chia dữ liệu thành tập huấn luyện, kiểm tra và xác thực (50:30:20)

train_size <- floor(0.5 * nrow(df))
test_size <- floor(0.3 * nrow(df))
val_size <- nrow(df) - train_size - test_size

train_data <- df[1:train_size, ]
test_data <- df[(train_size + 1):(train_size + test_size), ]
val_data <- df[(train_size + test_size + 1):nrow(df), ]

# Chuẩn bị dữ liệu cho huấn luyện mô hình
x_train <- train_data$index_normalized
y_train <- train_data$close

# Huấn luyện mô hình hồi quy tuyến tính
model <- lm(y_train ~ x_train)

# Dự đoán trên tập kiểm tra
x_test <- test_data$index_normalized
y_test <- test_data$close
y_pred_test <- predict(model, newdata = data.frame(x_train = x_test))

# Đánh giá lỗi trên tập kiểm tra (MAE, RMSE, MAPE)
mae_test <- mean(abs(y_test - y_pred_test))
rmse_test <- sqrt(mean((y_test - y_pred_test)^2))
mape_test <- mean(abs((y_test - y_pred_test) / y_test)) * 100

# Kết quả lỗi trên tập kiểm tra
cat("MAE (Test):", mae_test, "\n")
cat("RMSE (Test):", rmse_test, "\n")
cat("MAPE (Test):", mape_test, "\n")

# Dự đoán trên tập xác thực
x_val <- val_data$index_normalized
y_val <- val_data$close
y_pred_val <- predict(model, newdata = data.frame(x_train = x_val))

# Đánh giá lỗi trên tập xác thực (MAE, RMSE, MAPE)
mae_val <- mean(abs(y_val - y_pred_val))
rmse_val <- sqrt(mean((y_val - y_pred_val)^2))
mape_val <- mean(abs((y_val - y_pred_val) / y_val)) * 100

# Kết quả lỗi trên tập xác thực
cat("MAE (Validation):", mae_val, "\n")
cat("RMSE (Validation):", rmse_val, "\n")
cat("MAPE (Validation):", mape_val, "\n")

# Dự đoán cho 30 ngày tiếp theo
last_index <- max(df$index)
new_indices <- (last_index + 1):(last_index + 30)
new_indices_df <- data.frame(index = new_indices)
new_indices_normalized <- predict(scaler, new_indices_df)$index
future_predictions <- predict(model, newdata = data.frame(x_train = new_indices_normalized))

# Tạo data frame cho từng phần dữ liệu
train_data_plot <- data.frame(Index = train_data$index, Price = y_train, Type = "Train")
test_data_plot <- data.frame(Index = test_data$index, Price = y_test, Type = "Test")
validate_data_plot <- data.frame(Index = val_data$index, Price = y_val, Type = "Validate")
predictions_test_plot <- data.frame(Index = test_data$index, Price = y_pred_test, Type = "Predictions (Test)")
predictions_val_plot <- data.frame(Index = val_data$index, Price = y_pred_val, Type = "Predictions (Validate)")
next_30_days_plot <- data.frame(Index = new_indices, Price = future_predictions, Type = "Next 30 Days")

# Gộp tất cả vào một data frame cho ggplot
plot_data <- rbind(train_data_plot, test_data_plot, validate_data_plot, 
                   predictions_test_plot, predictions_val_plot, next_30_days_plot)

# Vẽ biểu đồ
ggplot(plot_data, aes(x = Index, y = Price, color = Type)) +
  geom_line() +
  labs(title = "Gold Price Predictions: Train, Test, Validate, and Next 30 Days",
       x = "Index", y = "Gold Price") +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "green", 
                                "Validate" = "purple", 
                                "Predictions (Test)" = "orange",
                                "Predictions (Validate)" = "pink",
                                "Next 30 Days" = "red")) +
  theme_minimal()

# Tạo data frame cho dự đoán 30 ngày tiếp theo và in ra kết quả
future_data <- data.frame(Day = new_indices, Predicted_Price = future_predictions)
print("Dự đoán giá vàng cho 30 ngày tiếp theo:")
print(future_data)



