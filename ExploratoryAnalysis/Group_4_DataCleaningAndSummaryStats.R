library(tidyverse)
library(vtable)

adultvars <- c("age", "workclass", "fnlwgt", "education", "educationnum", 
               "maritalstatus", "occupation", "relationship", "race", "sex", 
               "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "50k")

adult_raw <- read.csv("adult.data", header = FALSE, col.names = adultvars , skip = 1)

#Remove unwanted variable:
adult <- subset(adult_raw, select = -c(capitalgain, capitalloss, fnlwgt,
                                    race, nativecountry, education, relationship))

#sex and 50k binary:
#Female = 1, Male = 2
adult$sex <- as.factor(adult$sex)
adult$sex<- as.numeric(adult$sex) 

#<=50k = 1, >50k = 2
adult$X50k <- as.factor(adult$X50k)
adult$X50k <- as.numeric(adult$X50k)

#Re-code workclass, maritalstatus
adult <- adult %>%
  mutate(workclass = ifelse(workclass %in% 
                      c(" Federal-gov", " Local-gov", " State-gov"), 
                      " gov", workclass))

adult$maritalstatus = case_when(
  adult$maritalstatus %in% c(" Married-civ-spouse", " Married-AF-spouse", " Married-spouse-absent") ~ "Married",
  adult$maritalstatus %in% c(" Divorced", " Separated", " Widowed") ~ "DSW",
  adult$maritalstatus %in% c(" Never-married") ~ "NeverMarried",
)

#workclass, occupation filtering:
adult <- adult %>% 
  filter(!(workclass %in% c(" ?", " Without-pay", " Never-worked")))

adult <- adult %>%
  filter(!(occupation %in% c(" ?")))

#Add 3 to educationnum
adult$educationnum <- adult$educationnum + 3

## =========== SUMMARY STATISTICS ===========
adult = read.csv("Adult_NoOutlier.csv")

# Get the numeric variables
adult_numeric = adult[,c(1,3,7)]

sumtable(adult_numeric, out="latex")
sumtable(adult)
