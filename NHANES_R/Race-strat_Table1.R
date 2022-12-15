setwd("~/GitHub/NHANES_T2D/Data")
cleaned_full_df <- readRDS("cleaned_full_df.rds")


# Load relevant packages

library(tidyverse)
library(survey)
library(tableone)


# Keep those who are adults and non diabetics, while creating a triglycerides with logs.

cleaned_full_df_new <- cleaned_full_df |> 
  filter(ethnicity %in% c("White","Black") & age >= 18 & diabetic == 'no diabetes') |> 
  mutate(TG_log = log(TG))

cleaned_full_df_new$ethnicity <- droplevels(cleaned_full_df_new$ethnicity)
levels(cleaned_full_df_new$ethnicity)
# 
# cleaned_full_df_new$surveth<-paste(cleaned_full_df_new$survey_nr, cleaned_full_df_new$ethnicity, sep="")
# cleaned_full_df_new$surveth<-as.factor(cleaned_full_df_new$surveth)
# 
# 
# 
# # Apply survey design to the dataset
# 
# survey_df <- svydesign(data=cleaned_full_df_new, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
# 
# 
# # Create the table one, using tableone package 
# 
# k <- tableone::svyCreateTableOne(vars = c("gender","age","ethnicity","education","pregnancy","born_USA","waist","BMI","height","TG","LDL",
#                                 "TC","HDL","ever_lipid_meds","now_BP_meds","glucose","famhist_T2D","current_smoker","fasting_hr","active",
#                                 "SBP","DBP","hypertension_ever","hypertension_now","heart_disease","diabetic","TG_log","surveth"),                                  
#                                  strata = c('survey_nr','ethnicity'),
#                                  data = survey_df,
#                                  includeNA = T,
#                                  test = F, addOverall = F)
# #ethnicity
# options(max.print = 1000)
# 
# table1 <- print(k, varLabels = T, format = "p", showAllLevels = T, catDigits = 1, 
#       pDigits = 3, contDigits = 1, quote = T, missing = F, explain = F, printToggle = T, 
#       noSpaces = T, cramVars = NULL, dropEqual = T)


# We need to calculate missing values for the continuous values as well 
# (repeat for every survey year, did it manually)


cleaned_full_df_new_1999 <- cleaned_full_df_new |> filter(survey_nr == '1999-2000')
cleaned_full_df_new_2001 <- cleaned_full_df_new |> filter(survey_nr == '2001-2002')
cleaned_full_df_new_2003 <- cleaned_full_df_new |> filter(survey_nr == '2003_2004')
cleaned_full_df_new_2005 <- cleaned_full_df_new |> filter(survey_nr == '2005_2006')
cleaned_full_df_new_2007 <- cleaned_full_df_new |> filter(survey_nr == '2007_2008')
cleaned_full_df_new_2009 <- cleaned_full_df_new |> filter(survey_nr == '2009_2010')

survey_df_1999 <- svydesign(data=cleaned_full_df_new_1999, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
survey_df_2001 <- svydesign(data=cleaned_full_df_new_2001, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
survey_df_2003 <- svydesign(data=cleaned_full_df_new_2003, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
survey_df_2005 <- svydesign(data=cleaned_full_df_new_2005, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
survey_df_2007 <- svydesign(data=cleaned_full_df_new_2007, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
survey_df_2009 <- svydesign(data=cleaned_full_df_new_2009, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)


k <- tableone::svyCreateTableOne(vars = c("survey_nr", "gender","age","ethnicity","education","pregnancy","born_USA","waist","BMI","height","TG","LDL",
                                               "TC","HDL","ever_lipid_meds","now_BP_meds","glucose","famhist_T2D","current_smoker","fasting_hr","active",
                                               "SBP","DBP","hypertension_ever","hypertension_now","heart_disease","diabetic","TG_log"),
                                      data = survey_df_2009,
                                      strata = 'ethnicity',
                                      includeNA = T,
                                      test = T, addOverall = T)


print(k, varLabels = T, format = "p", showAllLevels = T, catDigits = 1,  pDigits = 3, contDigits = 1, quote = T, missing = T, explain = F, printToggle = T, 
                     noSpaces = T, cramVars = NULL, dropEqual = T)

#nonnormal = c("age","waist","BMI","height","TG","LDL",
#"TC","HDL","glucose","famhist_T2D","SBP","DBP"),

# White
df2 <- cleaned_full_df_new_1999[cleaned_full_df_new_1999$ethnicity == "White",] %>%
  gather(NameOfVar, Value) %>%
  group_by(NameOfVar) %>%
  summarize(Missingness = mean(is.na(Value)) * 100)

# Black
df2 <- cleaned_full_df_new_1999[cleaned_full_df_new_1999$ethnicity == "Black",] %>%
  gather(NameOfVar, Value) %>%
  group_by(NameOfVar) %>%
  summarize(Missingness = mean(is.na(Value)) * 100)
