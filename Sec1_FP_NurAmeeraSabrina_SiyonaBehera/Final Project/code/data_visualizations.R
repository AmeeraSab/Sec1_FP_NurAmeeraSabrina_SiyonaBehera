# Load libraries
library(tidyverse)

# Load cleaned + merged data
adult_income <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/adult_income.csv")
merged_summary <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/merged_summary.csv")

# 1. Bar Chart: Education vs Median Hours Worked
ggplot(education_hours_summary, aes(x = reorder(education, Median_Hours), y = Median_Hours)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Median Weekly Hours by Education Level",
    subtitle = "Ordered by increasing hours",
    x = "Education Level",
    y = "Median Hours Worked"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2. Bar Chart: Median Earnings by Major Group 
ggplot(merged_summary, aes(x = reorder(Major_Group, Median_Earnings), y = Median_Earnings, fill = Major_Group)) +
  geom_col() +
  labs(title = "Median Earnings by Major Group", x = "Major Group", y = "Median Earnings ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


#  3. Bar Chart: Average Weekly Hours by Major Group 
ggplot(merged_summary, aes(x = reorder(Major_Group, Avg_Hours), y = Avg_Hours, fill = Major_Group)) +
  geom_col() +
  labs(title =  "Hours Worked per Week by Major Group on Average", x = "Major Group", y = "Average Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

# 4. Boxplot: Hours Worked per Week by Sex
ggplot(adult_income, aes(x = sex, y = `hours-per-week`, fill = sex)) +
  geom_boxplot() +
  labs(title = "Hours Worked per Week by Sex",
       x = "Sex", y = "Hours per Week") +
  theme_minimal() +
  guides(fill = "none")

#  5. Boxplot: Distribution of Hours by Education Level 
ggplot(adult_income, aes(x = education, y = `hours-per-week`)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Hours Worked per Week by Education Level", x = "Education", y = "Hours per Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#  6. Boxplot: Distribution of Hours Worked Per Week by Job Type 
ggplot(adult_income, aes(x = workclass, y = `hours-per-week`, fill = workclass)) +
  geom_boxplot() +
  labs(title = "Hours Worked per Week by Workclass",
       x = "Workclass", y = "Hours per Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


# 7. Heatmap: Hours by Age Group and Education 
adult_income <- adult_income %>%
  mutate(Age_Group = cut(age, breaks = c(17, 25, 35, 45, 55, 65, 90), labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "66+")))

heatmap_data <- adult_income %>%
  group_by(Age_Group, education) %>%
  summarise(Avg_Hours = mean(`hours-per-week`, na.rm = TRUE))

ggplot(heatmap_data, aes(x = education, y = Age_Group, fill = Avg_Hours)) +
  geom_tile() +
  labs(title = "Average Hours by Age Group and Education",
       x = "Education", y = "Age Group", fill = "Avg Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + scale_fill_gradientn(colors = c("#fbb4b9", "#f768a1", "#ae017e"))






