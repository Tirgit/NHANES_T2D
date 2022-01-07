library(tidyverse)

# Set working directory & load data
setwd("~/GitHub/NHANES_T2D/NHANES_R")

# Create a small function to return probabilities from logits (coefficients)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# loop for bootstrapping starts here

# loop for multiple imputation (5 copies) start here
imp1 <- readRDS('imputed_df_1.rds')

###########################################
##### FRAMINGHAM OFFSPRING RISK SCORE #####
###########################################
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
  mutate(Risk_Framingham = Risk_Framingham * 0.01) |>
  select(-Framingham)


############################
##### DESIR RISK SCORE #####
############################

# risk score commented out, as there is a more precise regression model that we can use
# imp1 <- imp1 |> 
#   mutate(DESIR = ifelse(gender == 'male' & (waist >= 80 & waist < 90), 1, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'male' & (waist >= 90 & waist < 100), 2, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'male' &  waist >= 100, 3,0 )) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'male' &  current_smoker == 'smoker', 1, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'male' &  (SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds'),1, 0)) |>
#   mutate(DESIR = DESIR + ifelse(gender == 'female' & (waist >= 70 & waist < 80), 1, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'female' & (waist >= 80 & waist < 90), 2, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'female' &  waist >= 90, 3,0 )) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'female' &  famhist_T2D == 'family diabetes', 1, 0)) |> 
#   mutate(DESIR = DESIR + ifelse(gender == 'female' &  (SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds'),1, 0))


# We have to go to the regression coefficients to estimate the probabilities (we will also convert to probs)
imp1 <- imp1 |> 
  mutate(hypertension_desir = ifelse(SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds', 1, 0)) |> 
  mutate(Risk_DESIR = case_when(gender == 'male' ~ -10.45 + 0.72 * (current_smoker == 'smoker') + 0.081 * waist + 0.50 * (hypertension_desir == 1),
                                gender == 'female' ~ -11.81 + 1.09 * (famhist_T2D == 'family diabetes') +  0.095 * waist + 0.64 * (hypertension_desir == 1))) |>
  select(-hypertension_desir) |> # we don't need this column anymore, hence we delete
  mutate(Risk_DESIR = logit2prob(Risk_DESIR))


############################
##### EGATS RISK SCORE #####
############################

# Create the EGATS Score


imp1 <- imp1 |> 
  mutate(EGATS = ifelse(gender == "male", 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 45 & age < 50, 1, 0)) |> 
  mutate(EGATS = EGATS + ifelse(age >= 50, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 23 & BMI < 27.5, 3, 0)) |> 
  mutate(EGATS = EGATS + ifelse(BMI >= 27.5, 5, 0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'male' & waist >= 90, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(gender == 'female' & waist >= 80, 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(SBP >= 140 | DBP >= 90 | now_BP_meds == 'BP meds', 2, 0)) |> 
  mutate(EGATS = EGATS + ifelse(famhist_T2D == 'family diabetes', 4, 0))  |> 
  mutate(Risk_EGATS = ifelse(EGATS >= 6, 1, 0)) |>
  select(-EGATS)


###########################
##### ARIC RISK SCORE #####
###########################

## create ARIC model:
imp1 <- imp1 |> 
  mutate(ARIC = (-9.9808 + 0.0173 * age))|>
  mutate(ARIC = ARIC + 0.4433 * (ethnicity == "black"))|>
  mutate(ARIC = ARIC + 0.4981 * (famhist_T2D == 'family diabetes'))|>
  mutate(ARIC = ARIC + 1.5849 * glucose)|>
  mutate(ARIC = ARIC + 0.0111 * SBP)|>
  mutate(ARIC = ARIC + 0.0273 * waist)|>
  mutate(ARIC = ARIC - 0.0326 * height)|>
  mutate(ARIC = ARIC - 0.4718 * HDL) |>
  mutate(ARIC = ARIC + 0.2420 * TG) |>
  mutate(Risk_ARIC = logit2prob(ARIC)) |>
  select(-ARIC)

 

##################################
##### SAN ANTONIO RISK SCORE #####
##################################

imp1 <- imp1 |> 
  mutate(Antonio = (-13.415 + 0.028 * age))|>
  mutate(Antonio = Antonio + 0.661 * (gender == "female"))|>
  mutate(Antonio = Antonio + 0.412 * (ethnicity == "mexican"))|> 
  mutate(Antonio = Antonio + 0.079 * (glucose / 0.0555))|> #convert glucose into mm/dL = glucose / 0.0555 
  mutate(Antonio = Antonio + 0.018 *  SBP)|>
  mutate(Antonio = Antonio - 0.039 * (HDL / 0.0259))|> # convert HDL into mm/dL = HDL / 0.0259
  mutate(Antonio = Antonio + 0.070 *  BMI) |>
  mutate(Antonio = Antonio + 0.481 * (famhist_T2D == 'family diabetes')) |>
  mutate(Risk_Antonio = logit2prob(Antonio)) |>
  select(-Antonio)

  
imp1 <- imp1 |>
  select(survey_weight, survey_nr, gender, age, ethnicity,
         diabetic, Risk_Framingham, Risk_DESIR, Risk_EGATS,
         Risk_ARIC, Risk_Antonio)


##### LONG TO CALCULATE AVERAGE PREDICTED PROBABILITIES
##### 
##### CONSIDERATIONS:
##### WEIGHTS are 2-yr WEIGHTS - they are applicable within 
##### a specific 2-yr period (survey_nr)
#####
##### individuals who are diabetic should not be considered!!!
##### individuals under age 20 should not be considered!!!
##### category called "other race" should not be considered!!!
#####
##### Please calculate for the following 3 categories:
##### non-hispanic white (coded as is)
##### non-hispanic black (coded as is)
##### hispanic (coded as two categories for now: mexican american + other hispanic) - please merge these categories
#####
##### output:
##### we need race specific estimates in a new data frame
##### with columns:
##### survey_nr (year), ethnicity, estimate


# end of multiple imputation analysis loop

# pooling estimates from 5 imputed copies

# end of bootstrapping loop

# pooling estimates from 1,000 bootstraps



  