

# load data
df <- readRDS("C:/Users/vrw657/Documents/GitHub/NHANES_T2D/NHANES_R/imputed_df_1.rds")


###########################################
##### FRAMINGHAM OFFSPRING RISK SCORE #####
###########################################

# Set working directory

setwd("~/GitHub/NHANES_T2D/NHANES_R")

# Open the imputed  data frames

imp1 <- readRDS('imputed_df_1.rds')

library(tidyverse)

# We will assign new risk scores based on the Framingham scoring

# Glucose values in the table of the paper are in mg/dl, we need to convert into mmol/liter: 0.0555 * glucose table value

100 * 0.0555 # 5.55
126 * 0.0555 # 6.993

# Do the same for the HDL : 0.0259 * HDL table value

# Males :

40 * 0.0259 # 1.036

# Females

50 * 0.0259 # 1.295


# Triglyceride levels as well need to be multiplied by 0.0113

150 * 0.0113 # 1.695


# Let's create the Framingham score

imp1 <- imp1 |> 
  mutate(Framingham = ifelse(glucose >= 5.55, 10, 0)) |> 
  mutate(Framingham = Framingham + ifelse(BMI >= 25 & BMI <30, 2, 0)) |> 
  mutate(Framingham = Framingham + ifelse(BMI >= 30, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'male' & HDL < 1.036, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'female' & HDL < 1.295, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(famhist_T2D == 'family diabetes', 3, 0)) |> 
  mutate(Framingham = Framingham + ifelse(TG >= 1.695, 3, 0 )) |> 
  mutate(Framingham = Framingham + ifelse(SBP >= 130 | DBP >= 85 | now_BP_meds == 'BP meds', 2, 0)) |> 
  mutate(Risk_Framingham = case_when(Framingham <= 10 ~ 3,
                                     Framingham == 11 ~ 4,
                                     Framingham == 12 ~ 4,
                                     Framingham == 13 ~ 5,
                                     Framingham == 14 ~ 6,
                                     Framingham == 15 ~ 7,
                                     Framingham == 16 ~ 9,
                                     Framingham == 17 ~ 11,
                                     Framingham == 18 ~ 13,
                                     Framingham == 19 ~ 15,
                                     Framingham == 20 ~ 18,
                                     Framingham == 21 ~ 21,
                                     Framingham == 22 ~ 25,
                                     Framingham == 23 ~ 29,
                                     Framingham == 24 ~ 33,
                                     Framingham >= 25 ~ 35)) |> 
  mutate(Risk_Framingham = Risk_Framingham * 0.01)


############################
##### DESIR RISK SCORE #####
############################

imp1 <- imp1 |> 
  mutate(DESIR = ifelse(gender == 'male' & (waist >= 80 & waist < 90), 1, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'male' & (waist >= 90 & waist < 100), 2, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'male' &  waist >= 100, 3,0 )) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'male' &  current_smoker == 'smoker', 1, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'male' &  (SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds'),1, 0)) |>
  mutate(DESIR = DESIR + ifelse(gender == 'female' & (waist >= 70 & waist < 80), 1, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'female' & (waist >= 80 & waist < 90), 2, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'female' &  waist >= 90, 3,0 )) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'female' &  famhist_T2D == 'family diabetes', 1, 0)) |> 
  mutate(DESIR = DESIR + ifelse(gender == 'female' &  (SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds'),1, 0))



# Create a small function to return probabilities from logits (coefficients)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# We have to go to the regression coefficients to estimate the probabilities (we will also convert to probs)

imp1 <- imp1 |> 
  mutate(hypertension_desir = ifelse(SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds', 1, 0)) |> 
  mutate(Risk_DESIR = case_when(gender == 'male' ~ -10.45 + 0.72 * (current_smoker == 'smoker') + 0.081 * waist + 0.50 * (hypertension_desir == 1),
                                gender == 'female' ~ -11.81 + 1.09 * (famhist_T2D == 'family_diabetes') +  0.095 * waist + 0.64 * (hypertension_desir == 1))) |>
  select(-hypertension_desir) |> # we don't need this column anymore, hence we delete
  mutate(Risk_DESIR = logit2prob(Risk_DESIR))


############################
##### EGATS RISK SCORE #####
############################

# Create the EGATS Score

imp1 <- imp1 |> 
  mutate(EGATS = ifelse(gender == "male", 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 45 & age <= 49, 1, 0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 50, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 23 & BMI < 27.5, 3, 0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 27.5, 5, 0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'male' & waist >= 90, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'female' & waist >= 80, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds', 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(famhist_T2D == 'family diabetes', 4, 0))  

# Calculation of risk probabilities (extract logits then return probabilities see above for DESIR, intercept not specified clearly, 
# possible problem ?)


###########################
##### ARIC RISK SCORE #####
###########################

## create ARIC model:

#Pr(DM) = 1/(1 + e^−x), where x = ?

imp1 <- imp1 |> 
  mutate(ARIC = (-9.9808 + 0.0173 * age))|>
  mutate(ARIC = ARIC + 0.4433 * (ethnicity == "black"))|>
  mutate(ARIC = ARIC + 0.4981 * (famhist_T2D == 1))|>
  mutate(ARIC = ARIC + 1.5849 * glucose)|>
  mutate(ARIC = ARIC + 0.0111 * SBP)|>
  mutate(ARIC = ARIC + 0.0273 * waist)|>
  mutate(ARIC = ARIC - 0.0326 * height)|>
  mutate(ARIC = ARIC - 0.4718 * HDL) |>
  mutate(ARIC = ARIC + 0.2420 * TG)


# Needs calculation of risk probabilities (see DESIR on how to convert, intercept is specified on the paper)

 

##################################
##### SAN ANTONIO RISK SCORE #####
##################################

#1/(1-e^-x)?
# See above for that, same procedure

imp1 <- imp1 |> 
  mutate(Antonio = (-13.415 + 0.028 * age))|>
  mutate(Antonio = Antonio + 0.661 * (gender == "female"))|>
  mutate(Antonio = Antonio + 0.412 * (ethnicity == "mexican"))|> ##what about the other ethnicities?
  mutate(Antonio = Antonio + 0.079 * (glucose / 0.0555))|> #convert glucose into mm/dL = glucose / 0.0555 
  mutate(Antonio = Antonio + 0.018 *  SBP)|>
  mutate(Antonio = Antonio - 0.039 * (HDL / 0.0259))|> # convert HDL into mm/dL = HDL / 0.0259
  mutate(Antonio = Antonio + 0.070 *  BMI)|>
  mutate(Antonio = Antonio + 0.481 * (famhist_T2D == "1"))


############################
##### DPoRT RISK SCORE #####
############################
### Predict 9-y risk ###
imp1 <- imp1 |> 
  mutate(DPoRT.M = 10.5971 + ifelse(hypertension_now == "hypertension", -0.2624, 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(ethnicity == "white",0 , -0.6316)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(heart_disease == "heart disease",-0.5355 , 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(current_smoker == "smoker",-0.1765 , 0)) |> 
  mutate(DPoRT.M = DPoRT.M + ifelse(education %in% c("GED","some college","college"),0.2344 , 0)) |>
  mutate(DPoRT.M = DPoRT.M + ifelse(BMI<23 & age<45,0,
				ifelse(BMI<25 & age<45,-1.2378,
				ifelse(BMI<30 & age<45,-1.5490,
				ifelse(BMI<35 & age<45,-2.5437,
				ifelse(BMI>=35 & age<45,-3.4717,
				ifelse(BMI<23 & age>=45,-1.9794,
				ifelse(BMI<25 & age>=45,-2.4426,
				ifelse(BMI<30 & age>=45,-2.8488,
				ifelse(BMI<35 & age>=45,-3.3179,-3.5857))))))))) ) |>
  mutate(DPoRT.M = (log(365.25*9)-DPoRT.M)/0.8049 ) |> 
  mutate(DPoRT.M = 1-exp(-exp(DPoRT.M)) )

imp1 <- imp1 |> 
  mutate(DPoRT.F = 10.5474 + ifelse(hypertension_now == "hypertension",0 ,-0.2865)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(ethnicity == "white",-0.4309,0)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(born_USA == "immigrant",0,-0.2930)) |> 
  mutate(DPoRT.F = DPoRT.F + ifelse(education %in% c("GED","some college","college"),0.2042 , 0)) |>
  mutate(DPoRT.F = DPoRT.F + ifelse(BMI<23 & age<45,0,
				ifelse(BMI<25 & age<45,-0.5432,
				ifelse(BMI<30 & age<45,-0.8453,
				ifelse(BMI<35 & age<45,-1.4104,
				ifelse(BMI>=35 & age<45,-2.0483,
				ifelse(is.na(BMI)==TRUE & age<45,-1.1328,

				ifelse(BMI<23 & age<65,0.0711,
				ifelse(BMI<25 & age<65,-0.7011,
				ifelse(BMI<30 & age<65,-1.4167,
				ifelse(BMI<35 & age<65,-2.2150,
				ifelse(BMI>=35 & age<65,-2.2695,
				ifelse(is.na(BMI)==TRUE & age<65,-1.7260,


				ifelse(BMI<23 & age>=65,-1.0823,
				ifelse(BMI<25 & age>=65,-1.1419,
				ifelse(BMI<30 & age>=65,-1.5999,
				ifelse(BMI<35 & age>=65,-1.9254,
				ifelse(BMI>=35 & age>=65,-2.1959,-1.8284))))))))))))))))) ) |>
  mutate(DPoRT.F = (log(365.25*9)-DPoRT.F)/0.7814 ) |> 
  mutate(DPoRT.F = 1-exp(-exp(DPoRT.F)) )

imp1 <- imp1 |> 
  mutate(DPoRT = ifelse(gender == "male",DPoRT.M,DPoRT.F)) 



#########################
##### NS RISK SCORE #####
#########################



##############################
##### AUSDIAB RISK SCORE #####
##############################



############################
##### MJLPD RISK SCORE #####
############################





