library(dplyr)
library(e1071)

adult = read.csv("Adult_NoOutlier.csv")
adult = subset(adult, select = -c(X))

adult$X50k = as.factor(adult$X50k)
adult$workclass = as.factor(adult$workclass)
adult$maritalstatus = as.factor(adult$maritalstatus)
adult$occupation = as.factor(adult$occupation)
adult$sex = as.factor(adult$sex)

n_obs = nrow(adult)

# Set seed for reproducibility
set.seed(1)

# Use 20,000 training observations and the rest as test
n_train = 20000
n_test = n_obs - n_train

adult_train = adult[sample(1:n_train, n_train),]
adult_test = setdiff(adult, adult_train)
levels(adult_test$X50k) = levels(adult_train$X50k)

# Find an appropriate value of C to use in the model 
# The code takes a long time to run, so pick a smaller set of values
candidate_Cs = 2^seq(-5, 5, by = 1)

K = 10 # 10-fold CV
err_matrix = matrix(0, K, length(candidate_Cs))

# Loop across folds, which are rows of the matrix 
for (k in 1:K) {
  
  # For each fold, create a validation set and a learning set 
  # 20,000 obs in training set / 10 folds = 2000 in each fold 
  fold_lower_cutoff = 2000*(k-1) + 1
  fold_upper_cutoff = 2000*k
  
  valid_fold = adult_train[fold_lower_cutoff:fold_upper_cutoff, ]
  learn_fold = setdiff(adult_train, valid_fold)
  
  valid_features = valid_fold[,1:7]
  valid_Y = valid_fold[,8]
  
  learn_features = learn_fold[,1:7]
  learn_Y = learn_fold[,8]
  
  # Loop through each value in the candidate set 
  for (i in 1:length(candidate_Cs)) {
    print(sprintf("Currently on fold %d, iteration %d", k, i))
    current_C = candidate_Cs[i]
    
    # Fit a SVM model on the learning set using the current C
    current_svm = svm(X50k ~ ., data = learn_fold, type = "C-classification",
                      kernel = "linear", cost = current_C)
    
    # Create predictions on the validation set
    current_preds = predict(current_svm, valid_fold[,1:7])
    
    # Insert the mean test error in the matrix 
    err_matrix[k, i] = mean(current_preds != valid_Y)
    
  }
}
