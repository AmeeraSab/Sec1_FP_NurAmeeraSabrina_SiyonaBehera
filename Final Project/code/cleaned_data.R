# Load libraries
library(tidyverse)

# Load data
adult_income <- read_csv("/Users/siyona/STAT 184/Final Project/data/adult_income.csv")
college_majors <- read_csv("/Users/siyona/STAT 184/Final Project/data/college_majors.csv")

# Clean missing values
adult_income <- adult_income %>%
  mutate(across(where(is.character), ~na_if(., "?"))) %>%
  drop_na()

# Create a cleaned and ordered education level factor
adult_income <- adult_income %>%
  mutate(
    education_grouped = case_when(
      education %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th") ~ "Less than HS",
      education %in% c("HS-grad") ~ "High School",
      education %in% c("Some-college", "Assoc-acdm", "Assoc-voc") ~ "Some College or Associate",
      education %in% c("Bachelors") ~ "Bachelors",
      education %in% c("Masters") ~ "Masters",
      education %in% c("Doctorate", "Prof-school") ~ "Graduate or Professional",
      TRUE ~ "Other"
    ),
    education_grouped = factor(education_grouped, levels = c(
      "Less than HS", "High School", "Some College or Associate", 
      "Bachelors", "Masters", "Graduate or Professional", "Other"
    ))
  )

# Clean college majors
college_majors <- college_majors %>%
  rename(Major_Group = Major_category)

# View cleaned data
View(adult_income)
View(college_majors)

# Save cleaned data
write_csv(adult_income, "/Users/siyona/STAT 184/Final Project/data/cleaned_adult_income.csv")
write_csv(college_majors, "/Users/siyona/STAT 184/Final Project/data/cleaned_college_majors.csv")
