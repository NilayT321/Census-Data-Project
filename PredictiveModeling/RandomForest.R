library(dplyr)
library(magrittr)
library(randomForest)


set.seed(123)  # Set a seed for reproducibility
train_indices <- sample(1:nrow(adult2), 0.8 * nrow(adult2))
train_data <- adult2[train_indices, ]
test_data <- adult2[-train_indices, ] 


target_variable <- "X50k"
predictor_variables <- setdiff(names(train_data), target_variable)

# Train the random forest model
rf_model <- randomForest(formula = as.formula(paste(target_variable, "~ .")),
                         data = test_data,
                         ntree = 200)

# Make predictions on the test dataset
predictions <- predict(rf_model, newdata = test_data)

# Print the predictions
print(predictions)

print(rf_model)


# Extract the target variable from the testing dataset
Y_test_actual <- test_data$X50k

# Make predictions using the random forest model
Y_test_pred <- predict(rf_model, newdata = test_data)

# Calculate the mean squared error (MSE) as the test error
test_error <- mean((Y_test_actual - Y_test_pred)^2)

# Print or use the test error as needed
print(paste("Test Error (MSE):", test_error))

adult2$predicted_class <- predict(rf_model, newdata = adult2, type = "response")
