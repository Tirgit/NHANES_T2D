# Set working directory
setwd("~/GitHub/NHANES_T2D/NHANES_R")

# load necessary libraries
library(mice)

# load data
full_df <- readRDS("full_df_clean_missing.rds")

# bootstrap loop
for (i in 1:1000) {

bs_index <- sample(1:nrow(full_df), replace = TRUE)
bs_df <- full_df[bs_index,]

# 


print(i)
}









