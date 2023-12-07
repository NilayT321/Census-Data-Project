library(dplyr)
library(vtable)

adult = read.csv("Adult_NoOutlier.csv")
adult = subset(adult, select = -c(X))

# Get the numeric variables
adult_numeric = adult[,c(1,3,7)]

sumtable(adult_numeric, out="latex")
sumtable(adult)
