# load necessary libraries
library(mice)

# load data
full_df <- readRDS("C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/full_df_clean_missing.rds")



# MICE IMPUTATION
# save variables as vectors that are not needed for imputation
SEQN_vec <- full_df$SEQN
survey_weight_vec <- full_df$survey_weight
full_df$SEQN <- NULL
full_df$survey_weight <- NULL

imputation_object <- mice(full_df, method = "pmm", m = 5, maxit = 5)

SEQN <- 
  
  
  imputed_df <- complete(full_df_imp, 1)
plot(full_df_imp)
full_df_imp$method

saveRDS(full_df, "C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/full_df_clean_missing.rds")






