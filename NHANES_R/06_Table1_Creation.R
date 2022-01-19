# Set the working directory

setwd("~/GitHub/NHANES_T2D/Data")

# Load the full non-imputed dataset (Imputation is part of the estimation process, table 1 comes before that)

cleaned_full_df <- readRDS("cleaned_full_df.rds")


# Load relevant packages

library(tidyverse)
library(survey)
library(tableone)


# Keep those who are adults and non diabetics, while creating a tryglycerides with logs.

cleaned_full_df_new <- cleaned_full_df |> 
  filter(age >= 18 & diabetic == 'no diabetes') |> 
  mutate(TG_log = log(TG))


# Apply survey design to the dataset

survey_df <- svydesign(data=cleaned_full_df_new, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)


# Create the table one, using tableone package 

k <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'ethnicity', 'HDL', 
                                          'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                          'hypertension_now', 'TG_log'),
                                 strata = 'survey_nr', 
                                 data = survey_df,
                                 includeNA = T,
                                 test = F, addOverall = F)

options(max.print = 1000)

table1 <- print(k, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
      pDigits = 3, contDigits = 1, quote = F, missing = F, explain = F, printToggle = T, 
      noSpaces = T, cramVars = NULL, dropEqual = T)