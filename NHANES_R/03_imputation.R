# Set working directory
setwd("~/GitHub/NHANES_T2D/Data")

# load necessary libraries
library(mice)

# load complete merged data to obtain survey number levels
# nothing to do with this data, we only need the factor levels for the loop
cleaned_full_df <- readRDS("cleaned_full_df.rds")

# calculate average missingness in data
(sum(is.na(cleaned_full_df))/prod(dim(cleaned_full_df)))*100
# it is 15% - so we will make 15 imputed copies

# load data per survey (loop)
for (i in levels(cleaned_full_df$survey_nr)) {
  
full_df <- readRDS("full_",i,".rds")

# MICE IMPUTATION
# save variables as vectors that are not needed for imputation
SEQN <- full_df$SEQN
full_df$SEQN <- NULL

# imputation: 5 copies, 5 iterations, predictive mean matching algorithm
imputation_object <- mice(full_df, method = "rf", m = 15, maxit = 5, seed = 64370)

# investigate convergence visually
# imputation_object$method #those variables with no missing have "" as method - they are still used for imputation
# plot(imputation_object) #plots look OK

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

# re-merge ID 
imputed_df_1 <- as.data.frame(cbind(SEQN, imputed_1))
imputed_df_2 <- as.data.frame(cbind(SEQN, imputed_2))
imputed_df_3 <- as.data.frame(cbind(SEQN, imputed_3))
imputed_df_4 <- as.data.frame(cbind(SEQN, imputed_4))
imputed_df_5 <- as.data.frame(cbind(SEQN, imputed_5))


filename1 <- paste0("imputed_",i,"_1.rds")
filename2 <- paste0("imputed_",i,"_2.rds")
filename3 <- paste0("imputed_",i,"_3.rds")
filename4 <- paste0("imputed_",i,"_4.rds")
filename5 <- paste0("imputed_",i,"_5.rds")


# save imputed data
saveRDS(imputed_df_1, filename1)
saveRDS(imputed_df_2, filename2)
saveRDS(imputed_df_3, filename3)
saveRDS(imputed_df_4, filename4)
saveRDS(imputed_df_5, filename5)


}



