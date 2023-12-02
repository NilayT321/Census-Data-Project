#age
z_age <- scale(adult$age)
outliers_age_1 <- abs(z_age) > 3
outliers_obs_age_1 <- adult[outliers_age_1, ]

q1_age <- quantile(adult$age, 0.25)
q3_age <- quantile(adult$age, 0.75)
iqr_age <- q3_age - q1_age
outliers_age_2 <- (adult$age < (q1_age - 1.5 * iqr_age)) | (adult$age > (q3_age + 1.5 * iqr_age))
outliers_obs_age_2 <- adult[outliers_age_2, ]

hist_age_before <- hist(adult$age, main = "hist_age_before")

adult_age_2 <- adult %>%
  anti_join(outliers_obs_age_2, by = NULL)

hist_age_after <- hist(adult_age_2$age, main = "hist_age_after")

#educationnum
z_edu <- scale(adult$educationnum)
outliers_edu_1 <- abs(z_edu) > 3
outliers_obs_edu_1 <- adult[outliers_edu_1, ]

q1_edu <- quantile(adult$educationnum, 0.25)
q3_edu <- quantile(adult$educationnum, 0.75)
iqr_edu <- q3_edu - q1_edu
outliers_edu_2 <- (adult$educationnum < (q1_edu - 1.5 * iqr_edu)) | (adult$educationnum > (q3_edu + 1.5 * iqr_edu))
outliers_obs_edu_2 <- adult[outliers_edu_2, ]

hist_edu_before <- hist(adult$educationnum, main = "hist_edu_before") 

adult_edu <- adult %>%
  anti_join(outliers_obs_edu_1, by = NULL)

hist_edu_after <- hist(adult_edu$educationnum, main = "hist_edu_after") 

#hoursperweek
z_hpw <- scale(adult$hoursperweek)
outliers_hpw_1 <- abs(z_hpw) > 3
outliers_obs_hpw_1 <- adult[outliers_hpw_1, ]

q1_hpw <- quantile(adult$hoursperweek, 0.25)
q3_hpw <- quantile(adult$hoursperweek, 0.75)
iqr_hpw <- q3_hpw - q1_hpw
outliers_hpw_2 <- (adult$hoursperweek < (q1_hpw - 1.5 * iqr_hpw)) | (adult$hoursperweek > (q3_hpw + 1.5 * iqr_hpw))
outliers_obs_hpw_2 <- adult[outliers_hpw_2, ]

hist_hpw_before <- hist(adult$hoursperweek, main = "hist_hpw_before")

adult_hpw_1 <- adult %>%
  anti_join(outliers_obs_hpw_1, by = NULL)

hist_hpw_after_1 <- hist(adult_hpw_1$hoursperweek, main = "hist_hpw_after_1")

adult_hpw_2 <- adult %>%
  anti_join(outliers_obs_hpw_2, by = NULL)

hist_hpw_after_2 <- hist(adult_hpw_2$hoursperweek, main = "hist_hpw_after_2")

#remove outliers
adult <- adult %>%
  anti_join(outliers_obs_age_2, by = NULL)

adult <- adult %>%
  anti_join(outliers_obs_edu_2, by = NULL)

adult <- adult %>%
  anti_join(outliers_obs_hpw_1, by = NULL)
