

# load data
df <- readRDS("C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/imputed_df_1.rds")


###########################################
##### FRAMINGHAM OFFSPRING RISK SCORE #####
###########################################



############################
##### DESIR RISK SCORE #####
############################



############################
##### EGATS RISK SCORE #####
############################

setwd("~/GitHub/NHANES_T2D/NHANES_R") 
imp1 <- readRDS('imputed_df_1.rds')

imp1 <- imp1 |> 
  mutate(EGATS = ifelse(gender == "male", 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 45 & age <=49, 1,0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 50, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 23 & BMI <27.5, 3,0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 27.5, 5, 0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'male' & waist <= 90, 2,0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'female' & waist <= 80, 2,0)) |> 
  mutate(EGATS = EGATS + ifelse(SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds', 2,0)) |> 
  mutate(EGATS = EGATS + ifelse(famhist_T2D == 'family diabetes', 4,0))  

  

###########################
##### ARIC RISK SCORE #####
###########################

## create ARIC model:

#Pr(DM) = 1/(1 + e−x), where x = ?

imp1 <- imp1 |> 
  mutate(ARIC = (-9.9808 + 0.0173 * age))|>
  mutate(ARIC = ARIC + 0.4433 *(ethnicity==4))|>
  mutate(ARIC = ARIC + 0.5088 * (famhist_T2D ==1))|>
  mutate(ARIC = ARIC + 1.5849*glucose)|>
  mutate(ARIC = ARIC + 0.0111*SBP)|>
  mutate(ARIC = ARIC +0.0273*waist)|>
  mutate(ARIC = ARIC -0.0326*height)|>
  mutate(ARIC = ARIC -0.4718*HDL) |>
  mutate(ARIC = ARIC + 0.2420*TG)

 

##################################
##### SAN ANTONIO RISK SCORE #####
##################################



############################
##### DPoRT RISK SCORE #####
############################



#########################
##### NS RISK SCORE #####
#########################



##############################
##### AUSDIAB RISK SCORE #####
##############################



############################
##### MJLPD RISK SCORE #####
############################





