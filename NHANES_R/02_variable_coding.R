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
table(full_df$survey_nr, useNA = "always")
full_df$survey_nr <- as.factor(full_df$survey_nr)
full_df$survey_nr <- revalue(full_df$survey_nr, c("1"="1999-2000", 
                                                  "2"="2001-2002",
                                                  "3"="2003_2004",
                                                  "4"="2005_2006",
                                                  "5"="2007_2008",
                                                  "6"="2009_2010",
                                                  "7"="2011_2012",
                                                  "8"="2013_2014",
                                                  "9"="2015_2016",
                                                  "10"="2017_2018"))
table(full_df$survey_nr, useNA = "always")


###################
##### gender ######
###################
# survey weight for each 2 year data batch
# 1 = male
# 2 = female
table(full_df$gender, useNA = "always")
full_df$gender <- as.factor(full_df$gender)
full_df$gender <- revalue(full_df$gender, c("1"="male", "2"="female"))
table(full_df$gender, useNA = "always")


################
##### age ######
################
# numeric variable, nothing to do
summary(full_df$age)


######################
##### ethnicity ######
######################
# factor variable, coded as:
# 1 = Mexican American
# 2 = Other Hispanic
# 3 = Non-Hispanic White
# 4 = Non-Hispanic Black
# 5 = Other Race - Including Multi-Racial
table(full_df$ethnicity, useNA = "always")
full_df$ethnicity <- as.factor(full_df$ethnicity)
full_df$ethnicity <- revalue(full_df$ethnicity, c("1"="mexican", 
                                                  "2"="other_hispanic",
                                                  "3"="white",
                                                  "4"="black",
                                                  "5"="other"))
table(full_df$ethnicity, useNA = "always")


######################
##### education ######
######################
# factor variable, coded as:
# 1 = Less Than 9th Grade
# 2 = 9-11th Grade (Includes 12th grade with no diploma)
# 3 = High School Grad/GED or Equivalent
# 4 = Some College or AA degree
# 5 = College Graduate or above
# 7 = Refused
# 9 = Don't Know
# note - there are no level 6 and level 8 (not a mistake)
table(full_df$education, useNA = "always")
# setting those who refused and don't know to missing
full_df$education[full_df$education == 7] <- NA
full_df$education[full_df$education == 9] <- NA
full_df$education <- as.factor(full_df$education)
full_df$education <- revalue(full_df$education, c("1"="<9 grade", 
                                                  "2"="9-11 grade",
                                                  "3"="GED",
                                                  "4"="some college",
                                                  "5"="college"))
table(full_df$education, useNA = "always")









