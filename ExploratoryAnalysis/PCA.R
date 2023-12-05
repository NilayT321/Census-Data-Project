library(factoextra)

adult = read.csv("Adult_NoOutlier.csv")
adult = subset(adult, select = -c(X))

adult$X50k = as.factor(adult$X50k)
adult$workclass = as.factor(adult$workclass)
adult$maritalstatus = as.factor(adult$maritalstatus)
adult$occupation = as.factor(adult$occupation)
adult$sex = as.factor(adult$sex)

adultPCA = princomp(adult)