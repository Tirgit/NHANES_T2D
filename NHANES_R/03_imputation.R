# Set working directory
setwd("~/Documents/GitHub/NHANES_T2D/Data") # I need to use the Documents directory in MAC-OS

# load necessary libraries
library(mice)
library(tidyverse)


# load complete merged data to obtain survey number levels
# nothing to do with this data, we only need the factor levels for the loop
cleaned_full_df <- readRDS("cleaned_full_df.rds")

# Correctly fill the missing values of now_BP_meds variable

cleaned_full_df <- cleaned_full_df |> mutate(now_BP_meds = 
                                               if_else(hypertension_ever == 'no hypertension' & is.na(now_BP_meds), 
                                                       'no BP meds', as.character(now_BP_meds)))

# Recode the now_BP_meds variable as factor

cleaned_full_df$now_BP_meds <- as.factor(cleaned_full_df$now_BP_meds)

cleaned_full_df_used <- cleaned_full_df[cleaned_full_df$survey_nr %in% c("1999-2000","2001-2002","2003_2004","2005_2006","2007_2008","2009_2010"),]

# calculate average missingness in data
(sum(is.na(cleaned_full_df))/prod(dim(cleaned_full_df)))*100
# it is 11.9%, which we round up to 15% - so we will make 15 imputed copies

# load data per survey (loop)
for (i in levels(cleaned_full_df$survey_nr)) {
  
full_df <- readRDS(paste0("full_",i,".rds"))

# MICE IMPUTATION
# save variables as vectors that are not needed for imputation
SEQN <- full_df$SEQN
full_df$SEQN <- NULL
SDMVPSU <- full_df$SDMVPSU
full_df$SDMVPSU <- NULL
SDMVSTRA <- full_df$SDMVSTRA
full_df$SDMVSTRA <- NULL

# imputation: 5 copies, 5 iterations, random forest algorithm

# After changes in the mice
# We specify in mice that rfPackage = 'randomForest', otherwise we receive error of incorrect dimensions
imputation_object <- mice(full_df, rfPackage = 'randomForest', m = 15, maxit = 5, seed = 64370)

# investigate convergence visually
# imputation_object$method #those variables with no missing have "" as method - they are still used for imputation
# plot(imputation_object) #plots look OK


for (j in 1:15) {

# extract imputed datasets
imputed <- complete(imputation_object, j)

# removal of not needed variables
imputed$ever_lipid_meds <- NULL

# re-merge ID 
imputed_df <- as.data.frame(cbind(SEQN, SDMVPSU, SDMVSTRA, imputed))

filename <- paste0("imputed_",i,"_",j,".rds")

# save imputed data
saveRDS(imputed_df, filename)

  }
}



