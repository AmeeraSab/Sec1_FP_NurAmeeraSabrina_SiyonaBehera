# Load libraries
library(tidyverse)

# Load cleaned + merged data
adult_income <- read_csv("/Users/siyona/STAT 184/Final Project/data/adult_income.csv")
merged_summary <- read_csv("/Users/siyona/STAT 184/Final Project/data/merged_summary.csv")

# ---- 1. Bar Chart: Median Earnings by Major Group ----
ggplot(merged_summary, aes(x = reorder(Major_Group, Median_Earnings), y = Median_Earnings, fill = Major_Group)) +
  geom_col() +
  labs(title = "Median Earnings by Major Group", x = "Major Group", y = "Median Earnings ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


# ---- 2. Bar Chart: Average Weekly Hours by Major Group ----
ggplot(merged_summary, aes(x = reorder(Major_Group, Avg_Hours), y = Avg_Hours, fill = Major_Group)) +
  geom_col() +
  labs(title = "Average Hours Worked per Week by Major Group", x = "Major Group", y = "Average Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


# ---- 3. Scatterplot: Earnings vs Hours ----
ggplot(merged_summary, aes(x = Avg_Hours, y = Median_Earnings, color = Major_Group)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Relationship Between Hours Worked and Earnings", x = "Average Hours per Week", y = "Median Earnings ($)") +
  theme_minimal()

# ---- 4. Boxplot: Distribution of Hours by Education Level ----
ggplot(adult_income, aes(x = education, y = `hours-per-week`)) +
  geom_boxplot(fill = "#69b3a2") +
  labs(title = "Hours Worked per Week by Education Level", x = "Education", y = "Hours per Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# ---- 5. Violin Plot: Distribution of Hours by Workclass ----
ggplot(adult_income, aes(x = workclass, y = `hours-per-week`, fill = workclass)) +
  geom_violin() +
  labs(title = "Work Hours Distribution by Workclass", x = "Workclass", y = "Hours per Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

# ---- 6. Heatmap: Hours by Age Group and Education ----
adult_income <- adult_income %>%
  mutate(Age_Group = cut(age, breaks = c(17, 25, 35, 45, 55, 65, 90), labels = c("18–25", "26–35", "36–45", "46–55", "56–65", "66+")))

heatmap_data <- adult_income %>%
  group_by(Age_Group, education) %>%
  summarise(Avg_Hours = mean(`hours-per-week`, na.rm = TRUE))

ggplot(heatmap_data, aes(x = education, y = Age_Group, fill = Avg_Hours)) +
  geom_tile() +
  labs(title = "Average Hours by Age Group and Education", x = "Education", y = "Age Group", fill = "Avg Hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
