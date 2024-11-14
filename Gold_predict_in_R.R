# Cài đặt và tải các thư viện cần thiết
if (!require("tensorflow")) install.packages("tensorflow")
if (!require("keras3")) install.packages("keras3")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("readxl")) install.packages("readxl")
if (!require("Metrics")) install.packages("Metrics")

# Tải thư viện
library(tensorflow)
library(keras3)
library(ggplot2)
library(dplyr)
library(caret)
library(readxl)
library(Metrics)

# Hàm tính toán các chỉ số đánh giá mô hình
evaluate_model <- function(test, pred, val, pred_val) {
  # Kiểm tra xem các giá trị đầu vào có hợp lệ không
  if (length(test) != length(pred) || length(val) != length(pred_val)) {
    stop("Kích thước của test và pred, hoặc val và pred_val không tương thích.")
  }
  
  cat("** Testing **\n")
  # Tính toán MAE cho tập test
  test_mae <- mae(test, pred)
  cat("MAE: ", round(test_mae, 2), "\n")
  
  # Tính toán MAPE cho tập test (loại bỏ các giá trị bằng 0 để tránh lỗi chia cho 0)
  test_nonzero_indices <- test != 0
  if (any(test_nonzero_indices)) {
    test_ape <- abs((test[test_nonzero_indices] - pred[test_nonzero_indices]) / test[test_nonzero_indices])
    test_mape <- mean(test_ape) * 100
    cat("MAPE: ", round(test_mape, 2), "%\n")
  } else {
    cat("MAPE: Không thể tính do không có giá trị khác 0 trong tập test.\n")
  }
  
  # Tính toán RMSE cho tập test
  test_rmse <- rmse(test, pred)
  cat("RMSE: ", round(test_rmse, 2), "\n\n")
  
  cat("** Validation **\n")
  # Tính toán MAE cho tập validation
  val_mae <- mae(val, pred_val)
  cat("MAE: ", round(val_mae, 2), "\n")
  
  # Tính toán MAPE cho tập validation (loại bỏ các giá trị bằng 0 để tránh lỗi chia cho 0)
  val_nonzero_indices <- val != 0
  if (any(val_nonzero_indices)) {
    val_ape <- abs((val[val_nonzero_indices] - pred_val[val_nonzero_indices]) / val[val_nonzero_indices])
    val_mape <- mean(val_ape) * 100
    cat("MAPE: ", round(val_mape, 2), "%\n")
  } else {
    cat("MAPE: Không thể tính do không có giá trị khác 0 trong tập validation.\n")
  }
  
  # Tính toán RMSE cho tập validation
  val_rmse <- rmse(val, pred_val)
  cat("RMSE: ", round(val_rmse, 2), "\n")
}







# Đọc dữ liệu từ file Excel
file_path <- "C:/Users/Ken/PROJECT PTTK/R/Gold_data_filtered.xlsx"
df <- read_excel(file_path)
df
# Xử lý dữ liệu
df$date <- as.Date(df$date, format="%d-%m-%Y")
df1 <- dplyr::select(df, date, close)
colnames(df1) <- c("date", "Price")

# Vẽ biểu đồ giá vàng
p <- ggplot(df1, aes(x = date, y = Price)) +
  geom_line() +
  ggtitle("Prices of Gold") +
  xlab("Date") +
  ylab("Price")
print(p)

# Cài đặt và tải các gói cần thiết nếu chưa có
if (!require("caret")) install.packages("caret")
library(caret)


 
# Chuẩn hóa dữ liệu bằng MinMaxScaler với khoảng (0, 1)
preprocess_params <- preProcess(as.data.frame(df1$Price), method = c("range"), rangeBounds = c(0, 1))
sc_train <- predict(preprocess_params, as.data.frame(df1$Price))
sc_train <- sc_train[[1]]
  
# Kiểm tra kết quả chuẩn hóa
summary(sc_train)

# Chia dữ liệu thành tập train, test và validation
train_size <- round(0.7 * length(sc_train))
test_size <- round(0.2 * length(sc_train))
val_size <- length(sc_train) - train_size - test_size

train_data <- sc_train[1:train_size]
test_data <- sc_train[(train_size + 1):(train_size + test_size)]
val_data <- sc_train[(train_size + test_size + 1):length(sc_train)]

# Kiểm tra kích thước các tập dữ liệu
cat("Train size:", length(train_data), "\n")
cat("Test size:", length(test_data), "\n")
cat("Validation size:", length(val_data), "\n")



# Chuẩn bị tập dữ liệu huấn luyện cho mô hình
n_steps <- ifelse(length(train_data) > 100, 100, floor(length(train_data) / 2))
cat("Giá trị n_steps được điều chỉnh là:", n_steps, "\n")

x_train <- list()
y_train <- c()

if (length(train_data) > n_steps) {
  for (i in (n_steps + 1):length(train_data)) {
    x_train[[length(x_train) + 1]] <- train_data[(i - n_steps):(i - 1)]
    y_train <- c(y_train, train_data[i])
  }
  
  if (length(x_train) > 0) {
    x_train <- array(unlist(x_train), dim = c(length(x_train), n_steps, 1))
    y_train <- array(y_train, dim = c(length(y_train), 1))
    cat("Kích thước của x_train:", dim(x_train), "\n")
    cat("Kích thước của y_train:", dim(y_train), "\n")
  } else {
    cat("x_train không có phần tử nào để chuyển đổi!\n")
  }
} else {
  cat("train_data không đủ dài để tạo dữ liệu huấn luyện.\n")
}



# Cài đặt và tải keras3 nếu chưa có
if (!require("keras3")) install.packages("keras3")
library(keras3)
TF_ENABLE_ONEDNN_OPTS=0
# Giảm độ phức tạp của mô hình và thêm Dropout để tránh overfitting
model <- keras_model_sequential() %>%
  layer_lstm(units = 20, return_sequences = TRUE, input_shape = c(n_steps, 1)) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 20, return_sequences = TRUE) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 10) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_dense(units = 1)

# Compile mô hình với learning rate nhỏ hơn
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),  # Giảm learning rate
  loss = 'mean_absolute_error'
)

# Huấn luyện mô hình với early stopping và giảm learning rate khi không cải thiện
# Huấn luyện mô hình với các điều chỉnh để giảm MAPE hơn nữa
history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 40,  # Giảm số lượng epochs để tránh overfitting
  batch_size = 16,  # Giảm batch_size để cập nhật trọng số thường xuyên hơn
  validation_data = list(x_val, y_val),
  verbose = 2,
  callbacks = list(
    checkpoint,
    callback_early_stopping(monitor = 'val_loss', patience = 7),  # Dừng sớm nếu không cải thiện
    callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.5, patience = 3)  # Giảm learning rate khi không cải thiện
  )
)



# In lịch sử huấn luyện
print(history)



cat("Số dòng của dataset:", nrow(df1), "\n")



# Chuẩn bị tập dữ liệu test
x_test <- list()
y_test <- c()

for (i in (n_steps + 1):length(test_data)) {
  x_test[[length(x_test) + 1]] <- test_data[(i - n_steps):(i - 1)]
  y_test <- c(y_test, test_data[i])
}

x_test <- array(unlist(x_test), dim = c(length(x_test), n_steps, 1))
y_test <- array(y_test, dim = c(length(y_test), 1))

# Chuẩn bị tập dữ liệu validation
x_val <- list()
y_val <- c()

for (i in (n_steps + 1):length(val_data)) {
  x_val[[length(x_val) + 1]] <- val_data[(i - n_steps):(i - 1)]
  y_val <- c(y_val, val_data[i])
}

x_val <- array(unlist(x_val), dim = c(length(x_val), n_steps, 1))
y_val <- array(y_val, dim = c(length(y_val), 1))

# Dự đoán giá trị test, validation, và tương lai
pred_test <- model %>% predict(x_test)
pred_val <- model %>% predict(x_val)

# Chuẩn bị dữ liệu để dự đoán giá trong 30 ngày tiếp theo
future_data <- sc_train[(length(sc_train) - n_steps + 1):length(sc_train)]

x_future <- future_data
future_preds <- c()

for (i in 1:30) {
  x_future_array <- array(x_future, dim = c(1, n_steps, 1))
  future_pred <- model %>% predict(x_future_array)
  future_preds <- c(future_preds, future_pred)
  x_future <- c(x_future[-1], future_pred)
}

# Ngược chuẩn hóa dữ liệu để vẽ biểu đồ
sc_train_unscaled <- (sc_train * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_test_unscaled <- (y_test * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_val_unscaled <- (y_val * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
future_preds_unscaled <- (future_preds * (max(df1$Price) - min(df1$Price))) + min(df1$Price)

# Đánh giá mô hình
scaled_test <- y_test
scaled_test_pred <- pred_test
scaled_val <- y_val
scaled_val_pred <- pred_val

evaluate_model(scaled_test, scaled_test_pred, scaled_val, scaled_val_pred)

# In kết quả dự đoán tương lai
cat("\nDự đoán giá vàng trong 30 ngày tiếp theo:\n")
print(future_preds_unscaled)


# Kết hợp dữ liệu cho biểu đồ
train_unscaled <- sc_train_unscaled[1:train_size]
test_unscaled <- sc_train_unscaled[(train_size + 1):(train_size + test_size)]
val_unscaled <- sc_train_unscaled[(train_size + test_size + 1):length(sc_train)]
future_dates <- seq(max(df1$date) + 1, by = "days", length.out = 30)

# Tạo dataframe cho từng tập dữ liệu
train_df <- data.frame(Date = df1$date[1:train_size], Price = train_unscaled, Type = "Train")
test_df <- data.frame(Date = df1$date[(train_size + 1):(train_size + test_size)], Price = test_unscaled, Type = "Test")
val_df <- data.frame(Date = df1$date[(train_size + test_size + 1):length(sc_train)], Price = val_unscaled, Type = "Validate")
future_df <- data.frame(Date = future_dates, Price = future_preds_unscaled, Type = "Future Prediction")

# Kết hợp tất cả dữ liệu vào một dataframe
combined_df <- rbind(train_df, test_df, val_df, future_df)

# Vẽ biểu đồ
p <- ggplot(combined_df, aes(x = Date, y = Price, color = Type)) +
  geom_line(linewidth = 1) +  # Sử dụng linewidth thay cho size
  ggtitle("Gold Price Prediction: Train, Test, Validate, and Future") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Train" = "blue", "Test" = "orange", "Validate" = "green", "Future Prediction" = "red"))

print(p)

































# Chia dữ liệu thành tập train, test và validation (tỷ lệ 6-3-1)
train_size <- round(0.6 * length(sc_train))
test_size <- round(0.3 * length(sc_train))
val_size <- length(sc_train) - train_size - test_size

train_data <- sc_train[1:train_size]
test_data <- sc_train[(train_size + 1):(train_size + test_size)]
val_data <- sc_train[(train_size + test_size + 1):length(sc_train)]

# Kiểm tra kích thước các tập dữ liệu
cat("Train size:", length(train_data), "\n")
cat("Test size:", length(test_data), "\n")
cat("Validation size:", length(val_data), "\n")

# Các bước còn lại như cũ (không thay đổi)




# Chuẩn bị tập dữ liệu huấn luyện cho mô hình
n_steps <- ifelse(length(train_data) > 100, 100, floor(length(train_data) / 2))
cat("Giá trị n_steps được điều chỉnh là:", n_steps, "\n")

x_train <- list()
y_train <- c()

if (length(train_data) > n_steps) {
  for (i in (n_steps + 1):length(train_data)) {
    x_train[[length(x_train) + 1]] <- train_data[(i - n_steps):(i - 1)]
    y_train <- c(y_train, train_data[i])
  }
  
  if (length(x_train) > 0) {
    x_train <- array(unlist(x_train), dim = c(length(x_train), n_steps, 1))
    y_train <- array(y_train, dim = c(length(y_train), 1))
    cat("Kích thước của x_train:", dim(x_train), "\n")
    cat("Kích thước của y_train:", dim(y_train), "\n")
  } else {
    cat("x_train không có phần tử nào để chuyển đổi!\n")
  }
} else {
  cat("train_data không đủ dài để tạo dữ liệu huấn luyện.\n")
}



# Cài đặt và tải keras3 nếu chưa có
if (!require("keras3")) install.packages("keras3")
library(keras3)
TF_ENABLE_ONEDNN_OPTS=0
# Giảm độ phức tạp của mô hình và thêm Dropout để tránh overfitting
model <- keras_model_sequential() %>%
  layer_lstm(units = 20, return_sequences = TRUE, input_shape = c(n_steps, 1)) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 20, return_sequences = TRUE) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 10) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_dense(units = 1)

# Compile mô hình với learning rate nhỏ hơn
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),  # Giảm learning rate
  loss = 'mean_absolute_error'
)

# Huấn luyện mô hình với early stopping và giảm learning rate khi không cải thiện
# Huấn luyện mô hình với các điều chỉnh để giảm MAPE hơn nữa
history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 40,  # Giảm số lượng epochs để tránh overfitting
  batch_size = 16,  # Giảm batch_size để cập nhật trọng số thường xuyên hơn
  validation_data = list(x_val, y_val),
  verbose = 2,
  callbacks = list(
    checkpoint,
    callback_early_stopping(monitor = 'val_loss', patience = 7),  # Dừng sớm nếu không cải thiện
    callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.5, patience = 3)  # Giảm learning rate khi không cải thiện
  )
)



# In lịch sử huấn luyện
print(history)



cat("Số dòng của dataset:", nrow(df1), "\n")



# Chuẩn bị tập dữ liệu test
x_test <- list()
y_test <- c()

for (i in (n_steps + 1):length(test_data)) {
  x_test[[length(x_test) + 1]] <- test_data[(i - n_steps):(i - 1)]
  y_test <- c(y_test, test_data[i])
}

x_test <- array(unlist(x_test), dim = c(length(x_test), n_steps, 1))
y_test <- array(y_test, dim = c(length(y_test), 1))

# Chuẩn bị tập dữ liệu validation
x_val <- list()
y_val <- c()

for (i in (n_steps + 1):length(val_data)) {
  x_val[[length(x_val) + 1]] <- val_data[(i - n_steps):(i - 1)]
  y_val <- c(y_val, val_data[i])
}

x_val <- array(unlist(x_val), dim = c(length(x_val), n_steps, 1))
y_val <- array(y_val, dim = c(length(y_val), 1))

# Dự đoán giá trị test, validation, và tương lai
pred_test <- model %>% predict(x_test)
pred_val <- model %>% predict(x_val)

# Chuẩn bị dữ liệu để dự đoán giá trong 30 ngày tiếp theo
future_data <- sc_train[(length(sc_train) - n_steps + 1):length(sc_train)]

x_future <- future_data
future_preds <- c()

for (i in 1:30) {
  x_future_array <- array(x_future, dim = c(1, n_steps, 1))
  future_pred <- model %>% predict(x_future_array)
  future_preds <- c(future_preds, future_pred)
  x_future <- c(x_future[-1], future_pred)
}

# Ngược chuẩn hóa dữ liệu để vẽ biểu đồ
sc_train_unscaled <- (sc_train * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_test_unscaled <- (y_test * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_val_unscaled <- (y_val * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
future_preds_unscaled <- (future_preds * (max(df1$Price) - min(df1$Price))) + min(df1$Price)

# Đánh giá mô hình
scaled_test <- y_test
scaled_test_pred <- pred_test
scaled_val <- y_val
scaled_val_pred <- pred_val

evaluate_model(scaled_test, scaled_test_pred, scaled_val, scaled_val_pred)

# In kết quả dự đoán tương lai
cat("\nDự đoán giá vàng trong 30 ngày tiếp theo:\n")
print(future_preds_unscaled)


# Kết hợp dữ liệu cho biểu đồ
train_unscaled <- sc_train_unscaled[1:train_size]
test_unscaled <- sc_train_unscaled[(train_size + 1):(train_size + test_size)]
val_unscaled <- sc_train_unscaled[(train_size + test_size + 1):length(sc_train)]
future_dates <- seq(max(df1$date) + 1, by = "days", length.out = 30)

# Tạo dataframe cho từng tập dữ liệu
train_df <- data.frame(Date = df1$date[1:train_size], Price = train_unscaled, Type = "Train")
test_df <- data.frame(Date = df1$date[(train_size + 1):(train_size + test_size)], Price = test_unscaled, Type = "Test")
val_df <- data.frame(Date = df1$date[(train_size + test_size + 1):length(sc_train)], Price = val_unscaled, Type = "Validate")
future_df <- data.frame(Date = future_dates, Price = future_preds_unscaled, Type = "Future Prediction")

# Kết hợp tất cả dữ liệu vào một dataframe
combined_df <- rbind(train_df, test_df, val_df, future_df)

# Vẽ biểu đồ
p <- ggplot(combined_df, aes(x = Date, y = Price, color = Type)) +
  geom_line(linewidth = 1) +  # Sử dụng linewidth thay cho size
  ggtitle("Gold Price Prediction: Train, Test, Validate, and Future") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Train" = "blue", "Test" = "orange", "Validate" = "green", "Future Prediction" = "red"))

print(p)



























# Chia dữ liệu thành tập train, test và validation (tỷ lệ 5-2-1)
train_size <- round(0.5 * length(sc_train))
test_size <- round(0.2 * length(sc_train))
val_size <- length(sc_train) - train_size - test_size

train_data <- sc_train[1:train_size]
test_data <- sc_train[(train_size + 1):(train_size + test_size)]
val_data <- sc_train[(train_size + test_size + 1):length(sc_train)]

# Kiểm tra kích thước các tập dữ liệu
cat("Train size:", length(train_data), "\n")
cat("Test size:", length(test_data), "\n")
cat("Validation size:", length(val_data), "\n")



# Chuẩn bị tập dữ liệu huấn luyện cho mô hình
n_steps <- ifelse(length(train_data) > 100, 100, floor(length(train_data) / 2))
cat("Giá trị n_steps được điều chỉnh là:", n_steps, "\n")

x_train <- list()
y_train <- c()

if (length(train_data) > n_steps) {
  for (i in (n_steps + 1):length(train_data)) {
    x_train[[length(x_train) + 1]] <- train_data[(i - n_steps):(i - 1)]
    y_train <- c(y_train, train_data[i])
  }
  
  if (length(x_train) > 0) {
    x_train <- array(unlist(x_train), dim = c(length(x_train), n_steps, 1))
    y_train <- array(y_train, dim = c(length(y_train), 1))
    cat("Kích thước của x_train:", dim(x_train), "\n")
    cat("Kích thước của y_train:", dim(y_train), "\n")
  } else {
    cat("x_train không có phần tử nào để chuyển đổi!\n")
  }
} else {
  cat("train_data không đủ dài để tạo dữ liệu huấn luyện.\n")
}



# Cài đặt và tải keras3 nếu chưa có
if (!require("keras3")) install.packages("keras3")
library(keras3)
TF_ENABLE_ONEDNN_OPTS=0
# Giảm độ phức tạp của mô hình và thêm Dropout để tránh overfitting
model <- keras_model_sequential() %>%
  layer_lstm(units = 20, return_sequences = TRUE, input_shape = c(n_steps, 1)) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 20, return_sequences = TRUE) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_lstm(units = 10) %>%
  layer_dropout(rate = 0.2) %>%  # Thêm dropout
  layer_dense(units = 1)

# Compile mô hình với learning rate nhỏ hơn
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),  # Giảm learning rate
  loss = 'mean_absolute_error'
)

# Huấn luyện mô hình với early stopping và giảm learning rate khi không cải thiện
# Huấn luyện mô hình với các điều chỉnh để giảm MAPE hơn nữa
history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 40,  # Giảm số lượng epochs để tránh overfitting
  batch_size = 16,  # Giảm batch_size để cập nhật trọng số thường xuyên hơn
  validation_data = list(x_val, y_val),
  verbose = 2,
  callbacks = list(
    checkpoint,
    callback_early_stopping(monitor = 'val_loss', patience = 7),  # Dừng sớm nếu không cải thiện
    callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.5, patience = 3)  # Giảm learning rate khi không cải thiện
  )
)



# In lịch sử huấn luyện
print(history)



cat("Số dòng của dataset:", nrow(df1), "\n")



# Chuẩn bị tập dữ liệu test
x_test <- list()
y_test <- c()

for (i in (n_steps + 1):length(test_data)) {
  x_test[[length(x_test) + 1]] <- test_data[(i - n_steps):(i - 1)]
  y_test <- c(y_test, test_data[i])
}

x_test <- array(unlist(x_test), dim = c(length(x_test), n_steps, 1))
y_test <- array(y_test, dim = c(length(y_test), 1))

# Chuẩn bị tập dữ liệu validation
x_val <- list()
y_val <- c()

for (i in (n_steps + 1):length(val_data)) {
  x_val[[length(x_val) + 1]] <- val_data[(i - n_steps):(i - 1)]
  y_val <- c(y_val, val_data[i])
}

x_val <- array(unlist(x_val), dim = c(length(x_val), n_steps, 1))
y_val <- array(y_val, dim = c(length(y_val), 1))

# Dự đoán giá trị test, validation, và tương lai
pred_test <- model %>% predict(x_test)
pred_val <- model %>% predict(x_val)

# Chuẩn bị dữ liệu để dự đoán giá trong 30 ngày tiếp theo
future_data <- sc_train[(length(sc_train) - n_steps + 1):length(sc_train)]

x_future <- future_data
future_preds <- c()

for (i in 1:30) {
  x_future_array <- array(x_future, dim = c(1, n_steps, 1))
  future_pred <- model %>% predict(x_future_array)
  future_preds <- c(future_preds, future_pred)
  x_future <- c(x_future[-1], future_pred)
}

# Ngược chuẩn hóa dữ liệu để vẽ biểu đồ
sc_train_unscaled <- (sc_train * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_test_unscaled <- (y_test * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
y_val_unscaled <- (y_val * (max(df1$Price) - min(df1$Price))) + min(df1$Price)
future_preds_unscaled <- (future_preds * (max(df1$Price) - min(df1$Price))) + min(df1$Price)

# Đánh giá mô hình
scaled_test <- y_test
scaled_test_pred <- pred_test
scaled_val <- y_val
scaled_val_pred <- pred_val

evaluate_model(scaled_test, scaled_test_pred, scaled_val, scaled_val_pred)

# In kết quả dự đoán tương lai
cat("\nDự đoán giá vàng trong 30 ngày tiếp theo:\n")
print(future_preds_unscaled)


# Kết hợp dữ liệu cho biểu đồ
train_unscaled <- sc_train_unscaled[1:train_size]
test_unscaled <- sc_train_unscaled[(train_size + 1):(train_size + test_size)]
val_unscaled <- sc_train_unscaled[(train_size + test_size + 1):length(sc_train)]
future_dates <- seq(max(df1$date) + 1, by = "days", length.out = 30)

# Tạo dataframe cho từng tập dữ liệu
train_df <- data.frame(Date = df1$date[1:train_size], Price = train_unscaled, Type = "Train")
test_df <- data.frame(Date = df1$date[(train_size + 1):(train_size + test_size)], Price = test_unscaled, Type = "Test")
val_df <- data.frame(Date = df1$date[(train_size + test_size + 1):length(sc_train)], Price = val_unscaled, Type = "Validate")
future_df <- data.frame(Date = future_dates, Price = future_preds_unscaled, Type = "Future Prediction")

# Kết hợp tất cả dữ liệu vào một dataframe
combined_df <- rbind(train_df, test_df, val_df, future_df)

# Vẽ biểu đồ
p <- ggplot(combined_df, aes(x = Date, y = Price, color = Type)) +
  geom_line(linewidth = 1) +  # Sử dụng linewidth thay cho size
  ggtitle("Gold Price Prediction: Train, Test, Validate, and Future") +
  xlab("Date") +
  ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Train" = "blue", "Test" = "orange", "Validate" = "green", "Future Prediction" = "red"))

print(p)







