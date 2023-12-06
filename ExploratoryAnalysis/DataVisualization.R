
library(ggplot2)

# Example: Scatter plot of age vs. hoursperweek
ggplot(adult, aes(x = age, y = hoursperweek)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Hours per Week", x = "Age", y = "Hours per Week")

# Example: Box plot of hoursperweek by workclass
ggplot(adult, aes(x = workclass, y = hoursperweek)) +
  geom_boxplot() +
  labs(title = "Box Plot of Hours per Week by Workclass", x = "Workclass", y = "Hours per Week")

# Example: Histogram of age
ggplot(adult, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

# Example: Bar plot of educationnum
ggplot(adult, aes(x = factor(educationnum))) +
  geom_bar(fill = "salmon", color = "black") +
  labs(title = "Bar Plot of Education Number", x = "Education Number", y = "Count")

ggplot(adult, aes(x = hoursperweek, y = X50k)) +
  geom_point() +
  labs(title = "Scatter Plot of Hours per Week vs. X50k", x = "Hours per Week", y = "X50k")

ggplot(adult, aes(x = workclass, fill = factor(X50k))) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Workclass vs. X50k",
       x = "workclass",
       y = "Count") +
  scale_fill_manual(values = c("1" = "salmon", "2" = "black"))


library(dplyr)

# Create a new variable 'hours_per_week_category'
adult <- adult %>%
  mutate(hours_per_week_category = cut(hoursperweek, breaks = seq(0, 100, by = 10), labels = FALSE))

# Convert the new variable to a factor for better visualization
adult$hours_per_week_category <- factor(adult$hours_per_week_category, labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"))


ggplot(adult, aes(x = hours_per_week_category, fill = factor(X50k))) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Hours per Week Category vs. X50k",
       x = "Hours per Week Category",
       y = "Count") +
  scale_fill_manual(values = c("1" = "darkmagenta", "2" = "gold1"))
