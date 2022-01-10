# Set working directory
setwd("~/GitHub/NHANES_T2D")

# load necessary libraries
library(readxl)
result_df <- readRDS("NHANES_R/result_df.rds")
result_df$baseline_year <- NULL

# load incidence rates / 1000
diab_incidence <- read_xlsx("diab_incidence.xlsx")
diab_survival <- cbind(diab_incidence[,1], 1-diab_incidence[,2:5])
1000

# FRAMINGHAM, SAN ANTONIO: EIGHT YEAR INCIDENCES
# 2007 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:11) {
all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(8+i),2]))
hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(1+i):(8+i),3]))
white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(8+i),4]))
black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(8+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("8-yr-incidence",length_inc)
year_vals <- rep(2007:2018,4)
ethnicity_vals <- c(rep("All",12),rep("Hispanic",12),rep("White",12),rep("Black",12))
eight_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                 model = model_vals,
                                 ethnicity = ethnicity_vals,
                                 year = year_vals))
eight_yr_inc$avg_pred <- as.numeric(eight_yr_inc$avg_pred)
eight_yr_inc$year <- as.numeric(eight_yr_inc$year)


# DESIR, ARIC: NINE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:10) {
  all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(9+i),2]))
  hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(1+i):(9+i),3]))
  white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(9+i),4]))
  black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(9+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("9-yr-incidence",length_inc)
year_vals <- rep(2008:2018,4)
ethnicity_vals <- c(rep("All",11),rep("Hispanic",11),rep("White",11),rep("Black",11))
nine_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                    model = model_vals,
                                    ethnicity = ethnicity_vals,
                                    year = year_vals))
nine_yr_inc$avg_pred <- as.numeric(nine_yr_inc$avg_pred)
nine_yr_inc$year <- as.numeric(nine_yr_inc$year)



# EGATS: TWELVE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:7) {
  all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(12+i),2]))
  hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(12+i):(6+i),3]))
  white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(12+i),4]))
  black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(12+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("12-yr-incidence",length_inc)
year_vals <- rep(2011:2018,4)
ethnicity_vals <- c(rep("All",8),rep("Hispanic",8),rep("White",8),rep("Black",8))
twelve_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                   model = model_vals,
                                   ethnicity = ethnicity_vals,
                                   year = year_vals))
twelve_yr_inc$avg_pred <- as.numeric(twelve_yr_inc$avg_pred)
twelve_yr_inc$year <- as.numeric(twelve_yr_inc$year)




