# Load data from CSV file
data <- read.csv("D:/Thái/Phân tích thống kê/Platinum_data_filtered.csv")

# Select the 'close' column for analysis
close <- data$close

# Calculate Mean, Median, and Mode
mean_value <- mean(close, na.rm = TRUE)       
median_value <- median(close, na.rm = TRUE)   

# Mode calculation 
mode_value <- as.numeric(names(sort(table(close), decreasing = TRUE)[1]))  

# Calculate Range, Variance, and Standard Deviation
range_value <- range(close, na.rm = TRUE)    
variance_value <- var(close, na.rm = TRUE)    
std_dev_value <- sd(close, na.rm = TRUE)      

# Print calculated values
mean_value
median_value
mode_value
range_value
variance_value
std_dev_value

# Create a Histogram of the 'close' data
hist(close, 
     main = "Histogram of Closing Price of Platinum", 
     xlab = "Closing Price",                       
     col = "#c7675d",                              
     border = "black")  

# Create a Box Plot of the 'close' data
boxplot(close, 
        main = "Box Plot of Closing Price of Platinum",  
        ylab = "Closing Price",                     
        col = "#c7675d") 

# Load data from CSV file
data <- read.csv("D:/Thái/Phân tích thống kê/Platinum_data_filtered.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")
#data$close <- as.numeric(gsub(",", "", data$close))
# Create a Scatter Plot of Closing Price over Time
plot(data$date, data$close, 
     main = "Scatter Plot of Closing Price Over Time",  
     xlab = "Date",                                     
     ylab = "Closing Price",                            
     col = "#c7675d",                                   
     pch = 16)





