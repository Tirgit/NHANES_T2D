# Set the working directory
setwd("~/GitHub/NHANES_T2D/Data")


# Load relevant packages
library(tidyverse)
library(survey)
library(tableone)

# Create Rubin's Rules functions
# Rubin's Rules - pooling means
rubin_mean <- function(average) {
  Reduce("+", average) / length(average)
}

# Rubin's Rules - pooling SEs
rubin_se <- function(average, standard_error) {
  # Within imputation variance:
  within_var <- Reduce("+", lapply(standard_error, function(i){i*i})) / length(standard_error)
  # Between imputation variance:
  between_var <- Reduce("+", lapply(average, function(i){(i-rubin_mean(average))*(i-rubin_mean(average))})) / (length(average)-1)
  between_var2 <- between_var/ length(average)
  # Total variance:
  total_var <- within_var+between_var+between_var2
  # Pooled SE:
  sqrt(total_var)
}


# extract survey years
cleaned_full_df <- readRDS("cleaned_full_df.rds")
surveys <- levels(cleaned_full_df$survey_nr)
surveys <- surveys[1:6]



###############################################################
##### DESCRIPTIVE TABLE FOR CONTINUOUS VARIABLES (PER SURVEY)


means_survey <- list()
SDs_survey <- list()
cat_survey <- list()

means_list <- list()
ses_list <- list()
cat_list <- list()


for (i in 1:length(surveys)) {
for (m in 1:15) {
  
  cleaned_full_df <- readRDS(paste0("imputed_",surveys[i],"_", m, ".rds"))
  cleaned_full_df_new <- cleaned_full_df |> 
    filter(age >= 18 & diabetic == 'no diabetes') |> 
    mutate(TG_log = log(TG))
  survey_df <- svydesign(data=cleaned_full_df_new, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
  k <- tableone::svyCreateTableOne(vars = c('age',  'gender', 'glucose',  'BMI', 'ethnicity', 'HDL', 
                                            'now_BP_meds', 'famhist_T2D', 'current_smoker',  'SBP',  'DBP',  
                                            'hypertension_now', 'TG_log', 'height', 'waist'),
                                   data = survey_df,
                                   includeNA = T,
                                   test = F, addOverall = F)
  
  res_means <- as.matrix(k$ContTable[[1]][,4])
  res_ses <- as.matrix(k$ContTable[[1]][,5])/sqrt(nrow(cleaned_full_df_new))
  res_names <- names(k$ContTable[[1]][,1])
  means_list[[m]] <- res_means
  ses_list[[m]] <- res_ses
  cat_list[[m]] <- c(
    k$CatTable[[1]][[1]][,6],
    k$CatTable[[1]][[2]][,6],
    k$CatTable[[1]][[3]][,6],
    k$CatTable[[1]][[4]][,6],
    k$CatTable[[1]][[5]][,6],
    k$CatTable[[1]][[6]][,6]
  )
  
}
  
means_survey[[i]] <- rubin_mean(average = means_list)
SDs_survey[[i]] <- rubin_se(average = means_list, standard_error = ses_list)*sqrt(nrow(cleaned_full_df_new))
cat_survey[[i]] <- rubin_mean(average = cat_list)

}



lapply(means_survey,round,1)
lapply(SDs_survey,round,1)
lapply(cat_survey,round,1)

