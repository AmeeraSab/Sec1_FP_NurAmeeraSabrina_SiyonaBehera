# Load libraries
library(tidyverse)

# Load cleaned data
adult_income <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/cleaned_adult_income.csv")
college_majors <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/cleaned_college_majors.csv")

# Summary: Median hours worked by education level
education_hours_summary <- adult_income %>%
  group_by(education) %>%
  summarise(Median_Hours = median(`hours-per-week`, na.rm = TRUE)) %>%
  arrange(Median_Hours)


# Summarize average hours worked per week by Major Group
adult_summary <- adult_income %>%
  group_by(Major_Group) %>%
  summarise(Avg_Hours = mean(`hours-per-week`, na.rm = TRUE)) %>%
  arrange(desc(Avg_Hours))

# Summarize median earnings by Major Group
college_summary <- college_majors %>%
  group_by(Major_Group) %>%
  summarise(Median_Earnings = median(Median, na.rm = TRUE)) %>%
  arrange(desc(Median_Earnings))


# Merge the two summaries
merged_summary <- inner_join(adult_summary, college_summary, by = "Major_Group")
unique(college_majors$Major_Group)
intersect(unique(adult_income$Major_Group), unique(college_majors$Major_Group))

#View all tables in RStudio viewer
View(adult_summary)
View(college_summary)
View(merged_summary)
write_csv(merged_summary, "/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/merged_summary.csv")