library(dplyr)
library(e1071)

set.seed(2)
adult = read.csv("Adult_NoOutlier.csv")

adult$workclass = as.factor(adult$workclass)
adult$maritalstatus = as.factor(adult$maritalstatus)
adult$occupation = as.factor(adult$occupation)
adult$sex = as.factor(adult$sex)

# Recode the response to a logical 
# X50k = 1 (means <=50K, so mark it FALSE), other is TRUE
# adult$X50k = ifelse(adult$X50k == "1", FALSE, TRUE)

# The code is taking too long to run. We will use a smaller sample of the data
# Only take 3000 observations from the adult data set 
adult_sample = adult %>% sample_n(size = 3000)

# Standardize numerical features
adult_sample = adult_sample %>%
  mutate(age = (age - mean(age))/sd(age),
         educationnum = (educationnum - mean(educationnum))/sd(educationnum),
         hoursperweek = (hoursperweek - mean(hoursperweek))/sd(hoursperweek))

n_obs = nrow(adult_sample)

# Use an 70/30 split for the training and test sizes
n_train = 0.7 * n_obs
n_test = n_obs - n_train

adult_train = adult_sample[1:n_train, ]
adult_test = adult_sample[(n_train+1):n_obs,]

# Find an appropriate value of C to use in the model 
# The code takes a long time to run, so pick a smaller set of values
candidate_Cs = 2^seq(-7, 7, by = 1)

K = 5 # 10-fold CV
err_matrix = matrix(0, K, length(candidate_Cs))

# Matrix of folds 
folds = matrix(1:n_train, K)

# Loop across folds, which are rows of the matrix 
for (k in 1:K) {
  
  # For each fold, create a validation set and a learning set 
  # 20,000 obs in training set / 10 folds = 2000 in each fold 
  
  
  valid_ix = folds[k,]
  learn_ix = setdiff(1:n_train, folds[k,])
  
  valid_fold = adult_train[valid_ix,]
  learn_fold = adult_train[learn_ix,]
  
  # Loop through each value in the candidate set 
  for (i in 1:length(candidate_Cs)) {
    print(sprintf("Currently on fold %d, iteration %d", k, i))
    current_C = candidate_Cs[i]
    
    # Fit a SVM model on the learning set using the current C
    current_svm = svm(X50k ~ ., data = learn_fold, type = "C-classification",
                      kernel = "linear", cost = current_C)
    
    # Create predictions on the validation set
    current_preds = predict(current_svm, valid_fold)
    
    # Insert the mean test error in the matrix 
    err_matrix[k, i] = mean(current_preds != valid_fold$X50k)
    
  }
}

# Now, get the mean error for each value of C in candidate_C
mean_errs = apply(err_matrix, 2, mean)

# Get the minimum error
min_ix = which.min(mean_errs)

C_min = candidate_Cs[min_ix]

# Fit an SVM model with this data set on the entire training set. 
final_SVM = svm(X50k ~ ., data = adult_train, type = "C-classification",
                kernel = "linear", cost = C_min)

# Get predictions 
final_SVM_preds = predict(final_SVM, adult_test)

# Get the mean test error 
test_error = mean(final_SVM_preds != adult_test$X50k)

print(sprintf("The final test error for the SVM procedure was %.3f", test_error))
