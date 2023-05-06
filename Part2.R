#reading the data
df <- read.csv("uow_consumption1.csv", header = TRUE, sep = ",")

#Selecting the only needed columns from the csv file
df <- df[, c("date", "X18.00", "X19.00", "X20.00")]
df

#Checking for outliers
boxplot(df$X18.00)
boxplot(df$X19.00)
boxplot(df$X20.00)

#removing the records with NA
df1 <- df[complete.cases(df),]
df1



#Calcualte the interquartile range(IQR) for the 20:00 variable
q1 <- quantile(df1$X20.00, 0.25, na.rm = TRUE)
q3 <- quantile(df1$X20.00, 0.75, na.rm = TRUE)
iqr <- q3 - q1

#Calcualte the interquartile range(IQR) for the 18:00 variable
q1_18 <- quantile(df1$X18.00, 0.25, na.rm = TRUE)
q3_18 <- quantile(df1$X18.00, 0.75, na.rm = TRUE)
iqr_18 <- q3_18 - q1_18

#Calcualte the interquartile range(IQR) for the 19:00 variable
q1_19 <- quantile(df1$X19.00, 0.25, na.rm = TRUE)
q3_19 <- quantile(df1$X19.00, 0.75, na.rm = TRUE)
iqr_19 <- q3_19 - q1_19

# Set the range for outliers for 20:00
outlier_range <- 1.5 * iqr

# Set the range for outliers for 18:00
outlier_range_18 <- 1.5 * iqr_18

# Set the range for outliers for 19:00
outlier_range_19 <- 1.5 * iqr_19

# Identify the outliers and remove them
df1 <- df1[abs(df1$X20.00 - median(df1$X20.00, na.rm = TRUE)) <= outlier_range, ]
df1 <- df1[abs(df1$X18.00 - median(df1$X18.00, na.rm = TRUE)) <= outlier_range_18, ]
df1 <- df1[abs(df1$X19.00 - median(df1$X19.00, na.rm = TRUE)) <= outlier_range_19, ]

#Check if all outliers are removed
boxplot(df1$X18.00)
boxplot(df1$X19.00)
boxplot(df1$X20.00)

#Check the image of the dataframe
str(df1)


#selecting the number of time delayed inputs
#max_delay <- 2
#max_delay <- 7
#max_delay <- 4
#max_delay <- 4
#max_delay <- 2
#max_delay <- 3
#max_delay <- 3
#max_delay <- 4
#max_delay <- 7
#max_delay <- 4
#max_delay <- 4
#max_delay <- 4
max_delay <- 5

#For randomization
set.seed(123)

#Create the time delayed input vectors
input_vectors <- sapply(1:max_delay, function(i) lag(df1$X20.00, i))


# Remove the first max_delay rows containing NAs
input_vectors <- input_vectors[-(1:max_delay), ]

# Create output vector 
output_vector <- df1$X20.00[-(1:max_delay)]

# Construct input/output matrix with normalization
IO_matrix_norm <- cbind(scale(input_vectors), scale(output_vector))

#Creating an image of the IO_matrix
image(t(IO_matrix_norm), xlab = "Time Delay", ylab = "Sample Index")

# Load neuralnet package
library(neuralnet)

# Set the number of neurons in the hidden layer
#For Randomization
set.seed(123)
#num_hidden_neurons <- c(10,5)
#num_hidden_neurons <- c(12,6)
#num_hidden_neurons <- c(16,8,4)
#num_hidden_neurons <- c(14,7)
#num_hidden_neurons <- c(5,2)
#num_hidden_neurons <- c(15,5)
#num_hidden_neurons <- c(10)
#num_hidden_neurons <- c(10)
#num_hidden_neurons <- c(20,10)
#num_hidden_neurons <- c(20,10)
#num_hidden_neurons <- c(12,6)
#num_hidden_neurons <- c(12)
num_hidden_neurons <- c(20)

# Specify the formula for the neural network
formula <- as.formula(paste("output_vector ~", paste0("V", 1:max_delay, collapse = "+")))

# Train the neural network
neural_net <- neuralnet(formula, data = IO_matrix_norm, hidden = num_hidden_neurons)

#Plotting the NN 
plot(neural_net)

# Predict using the neural network
predictions <- predict(neural_net, IO_matrix_norm[,1:max_delay])

# Print the predictions
predictions

# Calculate the RMSE
RMSE <- sqrt(mean((predictions - output_vector)^2))

# Calculate the MAE
MAE <- mean(abs(predictions - output_vector))

# Calculate the MAPE
MAPE <- mean(abs((output_vector - predictions)/output_vector)) * 100

# Calculate the sMAPE
sMAPE <- mean(2 * abs(predictions - output_vector) / (abs(output_vector) + abs(predictions))) * 100


#printing the accuracy metrics 
cat("RMSE:", RMSE, "\n")
cat("MAE:", MAE, "\n")
cat("MAPE:", MAPE, "\n")
cat("sMAPE:", sMAPE, "\n")


# Ploting the best outcome as a Scatter plot for the prediction vs the real values
plot(output_vector, predictions, col = c("blue", "red"), main = "Scatter Plot of Real Values and Predictions Columns", 
     xlab = "Real Values", ylab = "Predictions")

# create a vector of names for the metrics
names <- c("RMSE", "MAE", "MAPE", "sMAPE")

# create a vector of the metric values
metric_values <- c(RMSE, MAE, MAPE, sMAPE)

# create a line chart of the metrics
plot(metric_values, type = "o", xlab = "Metrics", ylab = "Value", main = "Performance Metrics for Prediction", axes = FALSE)
axis(1, at = 1:length(names), labels = names)
axis(2)
box()
