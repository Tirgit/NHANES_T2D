# load necessary libraries
library(mice)

# load data
full_df <- readRDS("C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/full_df_clean_missing.rds")




















### POST-IMPUTATION

# remove not needed variables
full_df$fasting_hr <- NULL 
full_df$ever_lipid_meds <- NULL