setwd("~/GitHub/NHANES_T2D/Data")

cleaned_full_df <- readRDS("cleaned_full_df.rds")


# Load relevant packages

library(tidyverse)
library(survey)
library(tableone)


# Keep those who are adults and non diabetics, while creating a tryglycerides with logs.

levels(cleaned_full_df$survey_nr)

cleaned_full_df_new <- cleaned_full_df |> 
  filter(age >= 18 & diabetic == 'no diabetes') |> 
  mutate(TG_log = log(TG)) |> 
  filter(survey_nr %in% c('1999-2000', '2001-2002', '2003_2004', '2005_2006', '2007_2008', '2009_2010'))
  

cleaned_full_df_new |> 
  janitor::tabyl(ethnicity) |> 
  janitor::adorn_totals()


# Apply survey design to the dataset

survey_df <- svydesign(data=cleaned_full_df_new, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, 
                       nest=TRUE)


# Create the stratified table per ethnicity, using tableone package 

k_strat <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'HDL', 
                                                'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                                'hypertension_now', 'TG_log','height','waist'),
                                       strata = 'ethnicity', 
                                       data = survey_df,
                                       includeNA = T,
                                       test = F, addOverall = F)

options(max.print = 1000)

table_strat <-  print(k_strat, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
                      pDigits = 3, contDigits = 1, quote = F, missing = F, explain = F, printToggle = T, 
                      noSpaces = T, cramVars = NULL, dropEqual = T)


# We need to find missing data per ethnicity


cleaned_full_df_new_hispanics <- cleaned_full_df_new |> 
  filter(ethnicity == 'Hispanic')

cleaned_full_df_new_whites <- cleaned_full_df_new |> 
  filter(ethnicity == 'White')

cleaned_full_df_new_blacks <- cleaned_full_df_new |> 
  filter(ethnicity == 'Black')

cleaned_full_df_new_other <- cleaned_full_df_new |> 
  filter(ethnicity == 'Other')


# Create survey design for each ethnicity


survey_df_hispanics <- svydesign(data=cleaned_full_df_new_hispanics, id=~SDMVPSU, strata=~SDMVSTRA, 
                       weights=~survey_weight, nest=TRUE)

survey_df_whites <- svydesign(data=cleaned_full_df_new_whites, id=~SDMVPSU, strata=~SDMVSTRA, 
                                 weights=~survey_weight, nest=TRUE)

survey_df_blacks <- svydesign(data=cleaned_full_df_new_blacks, id=~SDMVPSU, strata=~SDMVSTRA, 
                                 weights=~survey_weight, nest=TRUE)

survey_df_other <- svydesign(data=cleaned_full_df_new_other, id=~SDMVPSU, strata=~SDMVSTRA, 
                                 weights=~survey_weight, nest=TRUE)


# Create table ones per ethnicity

# For Hispanics

options(survey.lonely.psu='adjust')


k_strat_hispanics <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'HDL', 
                                                'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                                'hypertension_now', 'TG_log','height','waist'),
                                       data = survey_df_hispanics,
                                       includeNA = T,
                                       test = F, addOverall = F)


options(max.print = 1000)

table_strat_hispanics <-  print(k_strat_hispanics, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
                      pDigits = 3, contDigits = 1, quote = F, missing = T, explain = F, printToggle = T, 
                      noSpaces = T, cramVars = NULL, dropEqual = T)


# For Whites

k_strat_whites <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'HDL', 
                                                          'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                                          'hypertension_now', 'TG_log','height','waist'),
                                                 data = survey_df_whites,
                                                 includeNA = T,
                                                 test = F, addOverall = F)


options(max.print = 1000)

table_strat_whites <-  print(k_strat_whites, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
                                pDigits = 3, contDigits = 1, quote = F, missing = T, explain = F, printToggle = T, 
                                noSpaces = T, cramVars = NULL, dropEqual = T)



# For Blacks

k_strat_blacks <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'HDL', 
                                                       'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                                       'hypertension_now', 'TG_log','height','waist'),
                                              data = survey_df_blacks,
                                              includeNA = T,
                                              test = F, addOverall = F)


options(max.print = 1000)

table_strat_blacks <-  print(k_strat_blacks, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
                             pDigits = 3, contDigits = 1, quote = F, missing = T, explain = F, printToggle = T, 
                             noSpaces = T, cramVars = NULL, dropEqual = T)


# For Other

k_strat_other <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'HDL', 
                                                       'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                                       'hypertension_now', 'TG_log','height','waist'),
                                              data = survey_df_other,
                                              includeNA = T,
                                              test = F, addOverall = F)


options(max.print = 1000)

table_strat_other <-  print(k_strat_other, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
                             pDigits = 3, contDigits = 1, quote = F, missing = T, explain = F, printToggle = T, 
                             noSpaces = T, cramVars = NULL, dropEqual = T)
