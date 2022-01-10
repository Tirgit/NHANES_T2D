# Set working directory
setwd("~/GitHub/NHANES_T2D/NHANES_R")

# load necessary libraries
library(mice)

# load data
full_df <- readRDS("full_df_clean_missing.rds")


# MICE IMPUTATION
# save variables as vectors that are not needed for imputation
SEQN <- full_df$SEQN
survey_weight <- full_df$survey_weight
full_df$SEQN <- NULL
full_df$survey_weight <- NULL

# imputation: 5 copies, 5 iterations, predictive mean matching algorithm
imputation_object <- mice(full_df, method = "pmm", m = 5, maxit = 5, seed = 64370)

# investigate convergence visually
imputation_object$method #those variables with no missing have "" as method - they are still used for imputation
plot(imputation_object) #plots look OK

# extract imputed datasets
imputed_1 <- complete(imputation_object, 1)
imputed_2 <- complete(imputation_object, 2)
imputed_3 <- complete(imputation_object, 3)
imputed_4 <- complete(imputation_object, 4)
imputed_5 <- complete(imputation_object, 5)

# removal of not needed variables
imputed_1$ever_lipid_meds <- NULL
imputed_2$ever_lipid_meds <- NULL
imputed_3$ever_lipid_meds <- NULL
imputed_4$ever_lipid_meds <- NULL
imputed_5$ever_lipid_meds <- NULL
imputed_1$fasting_hr <- NULL
imputed_2$fasting_hr <- NULL
imputed_3$fasting_hr <- NULL
imputed_4$fasting_hr <- NULL
imputed_5$fasting_hr <- NULL

# re-merge ID and weights
imputed_df_1 <- as.data.frame(cbind(SEQN, survey_weight, imputed_1))
imputed_df_2 <- as.data.frame(cbind(SEQN, survey_weight, imputed_2))
imputed_df_3 <- as.data.frame(cbind(SEQN, survey_weight, imputed_3))
imputed_df_4 <- as.data.frame(cbind(SEQN, survey_weight, imputed_4))
imputed_df_5 <- as.data.frame(cbind(SEQN, survey_weight, imputed_5))

# save imputed data
saveRDS(imputed_df_1, "imputed_df_1.rds")
saveRDS(imputed_df_2, "imputed_df_2.rds")
saveRDS(imputed_df_3, "imputed_df_3.rds")
saveRDS(imputed_df_4, "imputed_df_4.rds")
saveRDS(imputed_df_5, "imputed_df_5.rds")






