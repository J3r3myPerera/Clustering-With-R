#reading the data
df <- read.csv("uow_consumption1.csv", header = TRUE, sep = ",")

#Selecting the only needed columns from the csv file
df <- df[, c("date", "X18.00", "X19.00", "X20.00")]
df

#removing the records with NA
df1 <- df[complete.cases(df),]
df1

#Check the image of the dataframe
str(df1)

#Re-naming the columns
names(df)[names(df) == "X18.00"] <- "18:00"
names(df)[names(df) == "X19.00"] <- "19:00"
names(df)[names(df) == "X20.00"] <- "20:00"
head(df)


#selecting the number of time delayed inputs
max_delay <- 4

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
num_hidden_neurons <- c(12,6,3)

# Specify the formula for the neural network
formula <- as.formula(paste("output_vector ~", paste0("V", 1:max_delay, collapse = "+")))

# Train the neural network
neural_net <- neuralnet(formula, data = IO_matrix_norm, hidden = num_hidden_neurons)

plot(neural_net)

# Predict using the neural network
predictions <- predict(neural_net, IO_matrix_norm[,1:max_delay])

# Print the predictions
predictions

# Extract the actual values of the output vector
actual <- IO_matrix_norm[, max_delay + 1]

# Calculate the RMSE
RMSE <- sqrt(mean((predictions - actual)^2))
cat("RMSE:", RMSE, "\n")

# Calculate the MAE
MAE <- mean(abs(predictions - actual))
cat("MAE:", MAE, "\n")

# Calculate the MAPE
MAPE <- mean(abs((actual - predictions)/actual)) * 100
cat("MAPE:", MAPE, "\n")

# Calculate the sMAPE
sMAPE <- mean(2 * abs(predictions - actual) / (abs(actual) + abs(predictions))) * 100
cat("sMAPE:", sMAPE, "\n")

#Using the NARX method and Building the models

#Calling the nnet library
library(nnet)

#preparing the dataframe
library(caret)
trainIndex <- createDataPartition(df1$X20.00, p = 0.7, list = FALSE)
training <- df1[trainIndex, ]
testing <- df1[-trainIndex, ]

#Sorting the error of the NA 
df1 <- na.omit(df1)
df1 <- df1[]

#Creating the NARX neural network
formula <- X20.00 ~ X18.00 + X19.00 + lag(X20.00, -1) + lag(X20.00, -2) + lag(X18.00, -1) + lag(X18.00, -2) + lag(X19.00, -1) + lag(X19.00, -2)
neuralNNET <- nnet(formula, data = training, size=5, linout= TRUE)
predictions <- predict(neuralNNET, newdata = testing)

#plotting the neural network
# Load required packages
library(ggplot2)
# Extract the weights from the neural network
weights <- neuralNNET$wts

# Plot the neural network
ggplot() +
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 5, size = abs(weights[1,1]))) +
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 2, size = abs(weights[2,1]))) +
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 3, size = abs(weights[3,1]))) +
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 4, size = abs(weights[4,1]))) +
  geom_segment(aes(x = 2, y = 5, xend = 3, yend = 1, size = abs(weights[1,2]))) +
  geom_segment(aes(x = 2, y = 2, xend = 3, yend = 1, size = abs(weights[2,2]))) +
  geom_segment(aes(x = 2, y = 3, xend = 3, yend = 1, size = abs(weights[3,2]))) +
  geom_segment(aes(x = 2, y = 4, xend = 3, yend = 1, size = abs(weights[4,2]))) +
  geom_segment(aes(x = 3, y = 1, xend = 4, yend = 3, size = abs(weights[1,3]))) +
  geom_segment(aes(x = 3, y = 1, xend = 4, yend = 2, size = abs(weights[2,3]))) +
  geom_segment(aes(x = 3, y = 1, xend = 4, yend = 4, size = abs(weights[3,3]))) +
  geom_segment(aes(x = 3, y = 1, xend = 4, yend = 5, size = abs(weights[4,3]))) +
  scale_size_continuous(range = c(0, 10)) +
  xlim(0, 5) +
  ylim(0, 6) +
  theme_void()
