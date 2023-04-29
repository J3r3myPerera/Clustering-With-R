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
