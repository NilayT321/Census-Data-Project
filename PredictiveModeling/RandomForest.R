library(dplyr)
library(magrittr)


adult <- adult[sample(1:nrow(adult), nrow(adult)), ]

wordsadult <- c("age", "workclass", "educationnum", 
                "martialstatus", "occupation", "sex", 
                "hoursperweek", "X50k")

# Standardization
n <- nrow(adult)

Y <- as.factor(adult$X50k)
X <- as.data.frame(adult[, wordsadult])

X <- cbind(rep(1, n), X)

p <- ncol(X)

for (j in 2:p) {
  if (is.numeric(X[, j])) {
    X[, j] <- (X[, j] - mean(X[, j], na.rm = TRUE)) / sd(X[, j], na.rm = TRUE)
  }
}

# Analysis using random forest
set.seed(1)

reorder <- sample(1:n, n, replace = FALSE)

X <- X[reorder, ]
Y <- Y[reorder]

n_learn <- 500
n_test <- 2000

Y_test <- Y[1:n_test]
Y_learn <- Y[(n_test + 1):(n_test + n_learn)]

X_test <- X[1:n_test, ]
X_learn <- X[(n_test + 1):(n_test + n_learn), ]

library(randomForest)

# Constructs randomForest model
mdl <- randomForest(x = X_learn[, -1], y = Y_learn, ntree = 10, mtry = 3, nodesize = 10, importance = TRUE)

print(mdl)

Y_pred <- predict(mdl, newdata = X_test)

# Evaluate test error
rf_test_error <- sum(Y_test != Y_pred) / length(Y_test)

print("Test Error of Random Forest:")
print(rf_test_error)
