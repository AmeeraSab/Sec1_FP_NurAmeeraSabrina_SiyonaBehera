# Load libraries
library(tidyverse)

# Load data
adult_income <- read_csv("Final_Project/adult_income.csv")
college_majors <- read_csv("Final_Project/data/college_majors.csv")

# Clean missing values
adult_income <- adult_income %>%
  mutate(across(where(is.character), ~na_if(., "?"))) %>%
  drop_na()

# Create Major_Group
adult_income <- adult_income %>%
  mutate(Major_Group = case_when(
    str_detect(education, "Bachelors|Some-college") & str_detect(occupation, "Engineer|Tech-support|Craft-repair") ~ "Engineering",
    str_detect(education, "Bachelors|Some-college") & str_detect(occupation, "Exec-managerial|Sales|Adm-clerical|Protective-serv") ~ "Business",
    str_detect(education, "Masters|Bachelors|Some-college") & str_detect(occupation, "Prof-specialty|Health|Education|Social") ~ "Health",
    str_detect(education, "Prof-school") ~ "Law & Public Policy",
    str_detect(education, "Assoc-acdm|Assoc-voc") ~ "Industrial Arts & Consumer Services",
    str_detect(education, "HS-grad") ~ "Skilled Trades",
    TRUE ~ "Other"
  ))

# Clean college majors
college_majors <- college_majors %>%
  rename(Major_Group = Major_category)

# View cleaned data
View(adult_income)
View(college_majors)

# Save cleaned data
write_csv(adult_income, "Final_Project/cleaned_adult_income.csv")
write_csv(college_majors, "Final_Project/cleaned_college_majors.csv")
