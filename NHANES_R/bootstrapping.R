# Set working directory
setwd("~/GitHub/NHANES_T2D/NHANES_R")

# load necessary libraries
library(mice)

# load data
full_df <- readRDS("full_df_clean_missing.rds")

# initialize empty list to collect 500 bootstrap estimate data frames
estimate_list <- list()

# bootstrap loop
for (i in 1:500) {

bs_index <- sample(1:nrow(full_df), replace = TRUE)
bs_df <- full_df[bs_index,]

# save variables as vectors that are not needed for imputation
SEQN <- bs_df$SEQN
survey_weight <- bs_df$survey_weight
bs_df$SEQN <- NULL
bs_df$survey_weight <- NULL

# imputation: 5 copies, 5 iterations, predictive mean matching algorithm
imputation_object <- mice(bs_df, method = "pmm", m = 5, maxit = 5, seed = 64370)

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

# loop over imputed datasets to collect estimates

# initialize empty list to collect estimates
estimate_list_mi <- list()

for (j in 1:5) {
  imputed_df = get(paste0("imputed_df_", i))
  
  ##############################################
  # calculation of predicted risks (done)
  # Long's part: calculation of averages (in progress)
  # creating dataframe output (in progress)
  ##############################################
  
  estimate_list_mi[[j]] <- data_frame_with_results
}

# pool 5 estimate dfs and assign to list of 500 bootstrap dfs
estimate_list[[i]] <- Reduce("+", estimate_list_mi) / length(estimate_list_mi)

# print counter
print(paste0("Finished with bootstrap ",i))
}

# obtain 500 dataframe outputs from bootstrap
# generating median and 95% CI of estimate

# use lapply to get it for each row in the 500 list objects
quantile(x, probs = c(0.025, 0.5, 0.975))

