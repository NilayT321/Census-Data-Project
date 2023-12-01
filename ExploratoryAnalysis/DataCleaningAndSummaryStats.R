library(tidyverse)

adultvars <- c("age", "workclass", "fnlwgt", "education", "educationnum", 
               "martialstatus", "occupation", "relationship", "race", "sex", 
               "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "50k")

rawadult <- read.csv("adult.data", header = FALSE, col.names = adultvars , skip = 1)

adult <- subset(rawadult, select = -c(capitalgain, capitalloss, fnlwgt))

#Female = 1, Male = 2
adult$sex <- as.factor(adult$sex)
adult$sex<- as.numeric(adult$sex) 

#<=50k = 1, >50k = 2
adult$X50k <- as.factor(adult$X50k)
adult$X50k <- as.numeric(adult$X50k)

adult = adult %>% 
  filter(occupation != " ?" & workclass != " ?" & nativecountry != " ?") 

# Some people are volunteers (workclass = Without-pay)

adult$educationnum <- adult$educationnum + 3