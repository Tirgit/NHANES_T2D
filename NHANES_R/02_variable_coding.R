library(plyr)
library(dplyr)

full_df <- readRDS("C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/full_df.rds")

#################
##### SEQN ######
#################
# serial number, nothing to change

##########################
##### survey_weight ######
##########################
# survey weight for each 2 year data batch
# for 1999-2000 and 2001-2002, it is ALSO 2 year data weight, NOT the 4 year weight
# if data is actually merged, weights need to be recalculated
# but if we are analyzing per batch, weights are ready to be used
summary(full_df$survey_weight)

# removal of all individuals with NA as weight
full_df <- full_df[!is.na(full_df$survey_weight),]
summary(full_df$survey_weight)


######################
##### survey_nr ######
######################
# survey number, refers to data collection batch
# 1 = 1999-2000
# 2 = 2001-2002
# 3 = 2003_2004
# 4 = 2005_2006
# 5 = 2007_2008
# 6 = 2009_2010
# 7 = 2011_2012
# 8 = 2013_2014
# 9 = 2015_2016
# 10 = 2017_2018
full_df$survey_nr <- as.factor(full_df$survey_nr)
table(full_df$survey_nr)


###################
##### gender ######
###################
# survey weight for each 2 year data batch
table(full_df$gender)
full_df$gender <- as.factor(full_df$gender)
full_df$gender <- revalue(full_df$gender, c("1"="male", "2"="female"))
table(full_df$gender)







