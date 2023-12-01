library(tidyverse)

adultvars <- c("age", "workclass", "fnlwgt", "education", "educationnum", 
               "martialstatus", "occupation", "relationship", "race", "sex", 
               "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "50k")

adult <- read.csv("adult.data", header = FALSE, col.names = adultvars , skip = 1)

#Remove unwanted variable:
adult <- subset(adult, select = -c(capitalgain, capitalloss, fnlwgt,
                                    race, nativecountry, education, relationship))

#sex and 50k binary:
#Female = 1, Male = 2
adult$sex <- as.factor(adult$sex)
adult$sex<- as.numeric(adult$sex) 

#<=50k = 1, >50k = 2
adult$X50k <- as.factor(adult$X50k)
adult$X50k <- as.numeric(adult$X50k)

#Re-code workclass
adult <- adult %>%
  mutate(workclass = ifelse(workclass %in% 
                      c(" Federal-gov", " Local-gov", " State-gov"), 
                      " gov", workclass))

#workclass, occupation filtering:
adult <- adult %>% 
  filter(!(workclass %in% c(" ?", " Without-pay", " Never-worked")))

adult <- adult %>%
  filter(!(occupation %in% c(" ?")))

#Add 3 to educationnum
adult$educationnum <- adult$educationnum + 3
