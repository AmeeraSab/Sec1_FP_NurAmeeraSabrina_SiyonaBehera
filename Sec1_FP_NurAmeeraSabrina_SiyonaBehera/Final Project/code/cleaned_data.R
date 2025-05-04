# Load libraries
library(tidyverse)

# Load data
adult_income <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/adult_income.csv")
college_majors <- read_csv("/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/college_majors.csv")
head(adult_income)
# Clean missing values
adult_income <- adult_income %>%
  mutate(across(where(is.character), ~na_if(., "?"))) %>%
  drop_na()
# Combine all education levels < high school into one category
adult_income <- adult_income %>%
  mutate(education = case_when(
    education %in% c("Preschool", "1st-4th", "5th-6th", "7th-8th",
                     "9th", "10th", "11th", "12th") ~ "Less than HS",
    TRUE ~ education
  ))
# Create Major_Group column with correctly grouped job types and education levels
adult_income <- adult_income %>%
  mutate(
    Major_Group = case_when(
      str_detect(education, "Bachelors|Some-college") & str_detect(occupation, "Engineer|Tech-support|Craft-repair") ~ "Engineering",
      str_detect(education, "Bachelors|Some-college") & str_detect(occupation, "Exec-managerial|Sales|Adm-clerical|Protective-serv") ~ "Business",
      str_detect(education, "Masters|Bachelors|Some-college") & str_detect(occupation, "Prof-specialty|Health|Education|Social") ~ "Health",
      str_detect(education, "Prof-school") ~ "Law & Public Policy",
      str_detect(education, "Assoc-acdm|Assoc-voc") ~ "Industrial Arts & Consumer Services",
      TRUE ~ "Other"
    )
  )
#Dropping columns we don't need and only keeping relevant ones 
adult_income <- adult_income %>%
  select(education, `hours-per-week`, sex, age, race, workclass, Major_Group)

# Clean college majors
college_majors <- college_majors %>%
  rename(Major_Group = Major_category) %>%
  select(Major, Major_Group, Median)

# View cleaned data
View(adult_income)
View(college_majors)

# Save cleaned data
write_csv(adult_income, "/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/cleaned_adult_income.csv")
write_csv(college_majors, "/Users/siyona/STAT 184/Sec1_FP_NurAmeeraSabrina_SiyonaBehera/Final Project/data/cleaned_college_majors.csv")

