# Set working directory
setwd("~/GitHub/NHANES_T2D/NHANES_R")

# load necessary libraries
library(plyr)
library(dplyr)

# load data
full_df <- readRDS("full_df.rds")

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
# numeric variable, measurement unit: yrs
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


######################
##### pregnancy ######
######################
# factor variable, coded as:
# 1 = Yes, positive lab pregnancy test or self-reported pregnant at exam
# 2 = Not pregnant at exam
# 3 = Cannot ascertain if pregnant at exam
# note - we will exclude those pregnant at a later stage
table(full_df$pregnancy, useNA = "always")
# setting those with the third category  to missing
full_df$pregnancy[full_df$pregnancy == 3] <- NA
full_df$pregnancy <- as.factor(full_df$pregnancy)
full_df$pregnancy <- revalue(full_df$pregnancy, c("1"="pregnant", 
                                                  "2"="not pregnant"))
table(full_df$pregnancy, useNA = "always")


#####################
##### born_USA ######
#####################
# factor variable, coded three different ways over time
# first coding until 2006
# second coding 2007-2010
# third coding from 2011
# in all coding:
# 1 =Born in 50 US States or Washington, DC
# 2/3/4/5=other countries/places
# 7/77 = Refused
# 9/99 = Don't know
# recoding non-US born as a new category immigrants
# setting 7, 9, 99 to missing
table(full_df$born_USA, useNA = "always")
full_df$born_USA[full_df$born_USA == 3] <- 2
full_df$born_USA[full_df$born_USA == 4] <- 2
full_df$born_USA[full_df$born_USA == 5] <- 2
full_df$born_USA[full_df$born_USA == 7] <- NA
full_df$born_USA[full_df$born_USA == 9] <- NA
full_df$born_USA[full_df$born_USA == 77] <- NA
full_df$born_USA[full_df$born_USA == 99] <- NA
full_df$born_USA <- as.factor(full_df$born_USA)
full_df$born_USA <- revalue(full_df$born_USA, c("1"="born in USA", 
                                                  "2"="immigrant"))
table(full_df$born_USA, useNA = "always")


##################
##### waist ######
##################
# numeric variable, measurement unit: cm
summary(full_df$waist)


################
##### BMI ######
################
# numeric variable, measurement unit: kg/m2
summary(full_df$BMI)


###################
##### height ######
###################
# numeric variable, measurement unit: cm
summary(full_df$height)


#####################
##### SBP, DBP ######
#####################
# numeric variables, measurement unit: mmHg
# for systolic and diastolic blood pressures, 3 measurements
# were undertaken, and they should be averaged
# if one measurement failed, there is a 4th measurement
# I will average the 4 measurements (remove NAs)
# this is OK, as if 3 measurements are available, the 4th is missing
# otherwise, the 4th is available and another is missing
full_df$SBP <- rowMeans(as.data.frame(cbind(full_df$SBP1, full_df$SBP2, full_df$SBP3, full_df$SBP4)), na.rm = T)
full_df$DBP <- rowMeans(as.data.frame(cbind(full_df$DBP1, full_df$DBP2, full_df$DBP3, full_df$DBP4)), na.rm = T)
dropvars <- c("SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4")
full_df <- full_df[ , !(names(full_df) %in% dropvars)]
full_df$SBP[is.nan(full_df$SBP)] <- NA
full_df$DBP[is.nan(full_df$DBP)] <- NA
# further rules from NHANES:
# Systolic blood pressure cannot be greater than 300 mmHg
full_df$SBP[full_df$SBP > 300] <- NA
# Systolic blood pressure must be greater than diastolic blood pressure;
sum(full_df$DBP > full_df$SBP, na.rm = T) #it's OK
# If there is no systolic blood pressure, there can be no diastolic blood pressure;
full_df$DBP[is.na(full_df$SBP)] <- NA
# Diastolic blood pressure can be zero.
# all converted to numeric variables, original variables removed
summary(full_df$SBP)
summary(full_df$DBP)


###############################
##### TG (triglycerides) ######
###############################
# all lipids numeric, measurement unit: mmol/L
summary(full_df$TG)
summary(full_df$LDL)
summary(full_df$TC)
summary(full_df$HDL)
# for those rows with only LDL that is missing, we can use
# Friedewald formula to estimate LDL
# calculation is not valid if TG level is above 4.5 mmol/L
# calculation is only valid if fasted at least 8.5 hours or more but less than 24 hours
# LDL = Total cholesterol - (Triglyceride / 2.2) - HDL
friedewald_index <- !is.na(full_df$TG) & !is.na(full_df$TC) &
  !is.na(full_df$HDL) & full_df$TG <4.5 &
  is.na(full_df$LDL) & full_df$fasting_hr >= 8.5 &
  full_df$fasting_hr < 24 
friedewald_index[is.na(friedewald_index)] <- FALSE # NA is not allowed in this vector, as it will be used for subsetting
full_df$LDL[friedewald_index] <- full_df$TC[friedewald_index] - (full_df$TG[friedewald_index]/2.2) - full_df$HDL[friedewald_index]
# final values
summary(full_df$TG)
summary(full_df$LDL)
summary(full_df$TC)
summary(full_df$HDL)

############################
##### fasting glucose ######
############################
# numeric variable, measurement unit: mmol/L
summary(full_df$glucose)


##################
##### HbA1c ######
##################
# numeric variable, measurement unit: %
summary(full_df$hba1c)
# not in any prediction models, to be removed later

#######################################
##### family history of diabetes ######
#######################################
# factor variable, coded as:
# 1 = Yes
# 2 = No
# 7 = Refused
# 9 = Don't Know
table(full_df$famhist_T2D, useNA = "always")
# setting those with the category 7 and 9 to missing
full_df$famhist_T2D[full_df$famhist_T2D == 7] <- NA
full_df$famhist_T2D[full_df$famhist_T2D == 9] <- NA
full_df$famhist_T2D <- as.factor(full_df$famhist_T2D)
full_df$famhist_T2D <- revalue(full_df$famhist_T2D, c("1"="family diabetes", 
                                                "2"="no family diabetes"))
table(full_df$famhist_T2D, useNA = "always")


##############################
##### ever hypertension ######
##############################
# factor, we need to generate this variable
full_df$hypertension_ever <- 0
# criteria 1: self reported ever told hypertensive
# in the original variable, 1 = yes (doctor told have hypertension)
full_df$hypertension_ever[full_df$ever_hypertension == 1] <- 1
full_df$ever_hypertension <- NULL
# criteria 2: ever taking blood pressure meds
# in the original variable, 1 = yes (told before to take meds)
full_df$hypertension_ever[full_df$ever_BP_meds == 1] <- 1
full_df$ever_BP_meds <- NULL
# criteria 3: now taking blood pressure meds
# in the original variable, 1 = yes (told before to take meds)
full_df$hypertension_ever[full_df$now_BP_meds == 1] <- 1
# criteria 4: SBP over 130
full_df$hypertension_ever[full_df$SBP > 130] <- 1
# criteria 5: DBP over 80
full_df$hypertension_ever[full_df$DBP > 80] <- 1
# convert to factor
table(full_df$hypertension_ever, useNA = "always")
full_df$hypertension_ever <- as.factor(full_df$hypertension_ever)
full_df$hypertension_ever <- revalue(full_df$hypertension_ever, c("0"="no hypertension", 
                                                "1"="hypertension"))
table(full_df$hypertension_ever, useNA = "always")


#################################
##### current hypertension ######
#################################
# factor, we need to generate this variable
full_df$hypertension_now <- 0
# criteria 1: now taking blood pressure meds
# in the original variable, 1 = yes (told before to take meds)
full_df$hypertension_now[full_df$now_BP_meds == 1] <- 1
# criteria 2: SBP over 130
full_df$hypertension_now[full_df$SBP > 130] <- 1
# criteria 3: DBP over 80
full_df$hypertension_now[full_df$DBP > 80] <- 1
# convert to factor
table(full_df$hypertension_now, useNA = "always")
full_df$hypertension_now <- as.factor(full_df$hypertension_now)
full_df$hypertension_now <- revalue(full_df$hypertension_now, c("0"="no hypertension", 
                                                                  "1"="hypertension"))
table(full_df$hypertension_now, useNA = "always")


#######################################
##### lipid lowering medications ######
#######################################
# factor variable, coded as:
# 1 = Yes
# 2 = No
# 7 = Refused
# 9 = Don't Know
table(full_df$ever_lipid_meds, useNA = "always")
# setting those with the category 7 and 9 to missing
full_df$ever_lipid_meds[full_df$ever_lipid_meds == 7] <- NA
full_df$ever_lipid_meds[full_df$ever_lipid_meds == 9] <- NA
full_df$ever_lipid_meds <- as.factor(full_df$ever_lipid_meds)
full_df$ever_lipid_meds <- revalue(full_df$ever_lipid_meds, c("1"="lipid meds", 
                                                      "2"="no lipid meds"))
table(full_df$ever_lipid_meds, useNA = "always")


###############################################
##### current antihypertensive treatment ######
###############################################
# factor variable, coded as:
# 1 = Yes
# 2 = No
# 7 = Refused
# 9 = Don't Know
table(full_df$now_BP_meds, useNA = "always")
# setting those with the category 7 and 9 to missing
full_df$now_BP_meds[full_df$now_BP_meds == 7] <- NA
full_df$now_BP_meds[full_df$now_BP_meds == 9] <- NA
full_df$now_BP_meds <- as.factor(full_df$now_BP_meds)
full_df$now_BP_meds <- revalue(full_df$now_BP_meds, c("1"="BP meds", 
                                                      "2"="no BP meds"))
table(full_df$now_BP_meds, useNA = "always")


##########################
##### heart disease ######
##########################
# factor, we need to generate this variable
full_df$heart_disease <- 0
# criteria 1: ever heart failure
# in the original variable, 1 = yes
full_df$heart_disease[full_df$ever_heartfailure == 1] <- 1
full_df$ever_heartfailure <- NULL
# criteria 2: ever CHD
# in the original variable, 1 = yes
full_df$heart_disease[full_df$ever_chd == 1] <- 1
full_df$ever_chd <- NULL
# criteria 3: ever angina
# in the original variable, 1 = yes
full_df$heart_disease[full_df$ever_angina == 1] <- 1
full_df$ever_angina <- NULL
# criteria 4: ever heart attack
# in the original variable, 1 = yes
full_df$heart_disease[full_df$ever_heartattack == 1] <- 1
full_df$ever_heartattack <- NULL
# criteria 5: ever stroke
# in the original variable, 1 = yes
full_df$heart_disease[full_df$ever_stroke == 1] <- 1
full_df$ever_stroke <- NULL
# convert to factor
table(full_df$heart_disease, useNA = "always")
full_df$heart_disease <- as.factor(full_df$heart_disease)
full_df$heart_disease <- revalue(full_df$heart_disease, c("0"="no heart disease", 
                                                                "1"="heart disease"))
table(full_df$heart_disease, useNA = "always")


####################
##### smoking ######
####################
# survey number, refers to data collection batch
# 1 = Every day
# 2 = Some days
# 3 = Not at all
# 7 = Refused
# 9 = Don't know
table(full_df$current_smoker, useNA = "always")
# recode so that 1 = smoker, 2 = nonsmoker
full_df$current_smoker[full_df$current_smoker == 2] <- 1
table(full_df$current_smoker, useNA = "always")
full_df$current_smoker[full_df$current_smoker == 3] <- 2
full_df$current_smoker <- as.factor(full_df$current_smoker)
full_df$current_smoker <- revalue(full_df$current_smoker, c("1"="smoker", 
                                                  "2"="non-smoker"))
table(full_df$current_smoker, useNA = "always")


############################
##### diabetes status ######
############################
# we need to generate this variable for exclusion
# as we only need diabetes-free individuals for risk prediction
# however, we don't want to assign diabetic based on glucose
# or HbA1c - as these individuals are undiagnosed
# i.e. they should contribute to future incidence rates
full_df$diabetic <- 0
# criteria 1: self reported diabetes (doctor told).
# in the original variable, 1 = yes (doctor diagnosed diabetes)
full_df$diabetic[full_df$ever_diabetes == 1] <- 1
full_df$ever_diabetes <- NULL
# criteria 2: taking insulin
# in the original variable, 1 = yes (taking insulin)
full_df$diabetic[full_df$insulin == 1] <- 1
full_df$insulin <- NULL
# criteria 3: taking oral diabetes medication
# in the original variable, 1 = yes (taking oral diabetic medication)
full_df$diabetic[full_df$oral_diab_med == 1] <- 1
full_df$oral_diab_med <- NULL
# criteria 4: fasting glucose above 7 mmol/L
# full_df$diabetic[full_df$glucose > 7] <- 1
# criteria 5: HbA1c above 6.5%
# full_df$diabetic[full_df$hba1c > 6.5] <- 1
full_df$hba1c <- NULL
# tabulate diabetes and convert to factor
table(full_df$diabetic, useNA = "always")
full_df$diabetic <- as.factor(full_df$diabetic)
full_df$diabetic <- revalue(full_df$diabetic, c("0"="no diabetes", 
                                                "1"="diabetes"))
table(full_df$diabetic, useNA = "always")

saveRDS(full_df, "full_df_clean_missing.rds")

