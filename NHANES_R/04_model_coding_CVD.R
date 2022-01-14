library(tidyverse)

# Set working directory & load data
setwd("~/GitHub/NHANES_T2D/Data")

# Create a small function to return probabilities from logits (coefficients)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# load example data
imp1 <- readRDS("imputed_1999-2000_1.rds")
  

#############################
##### PCE (ASCVD) SCORE #####
#############################

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


#################################
##### FRAMINGHAM RISK SCORE #####
#################################

xxxxx



#################################
##### SCORE RISK ESTIMATION #####
#################################

# age needs to be more than 20
imp1 <- imp1[imp1$age > 20,]

imp1 <- imp1 |> 
  mutate(alpha_chd = ifelse(gender == "female", -28.7, -21.0)) |>
  mutate(p_chd = ifelse(gender == "female", 6.23, 4.62)) |>
  mutate(s0age_chd = exp(-(exp(alpha_chd))*(age-20)^p_chd)) |>
  mutate(s0age10_chd = exp(-(exp(alpha_chd))*(age-10)^p_chd)) |>
  mutate(alpha_noncvd_chd = ifelse(gender == "female", -30.0, -25.7)) |>
  mutate(p_noncvd_chd = ifelse(gender == "female", 6.42, 5.47)) |>
  mutate(s0age_noncvd_chd = exp(-(exp(alpha_noncvd_chd))*(age-20)^p_noncvd_chd)) |>
  mutate(s0age10_noncvd_chd = exp(-(exp(alpha_noncvd_chd))*(age-10)^p_noncvd_chd)) |>
  mutate(w_chd = 0.24*(TC-6)+0.018*(SBP-120)) |>
  mutate(w_chd = w_chd + ifelse(current_smoker == "smoker", 0.71, 0)) |>
  mutate(w_noncvd_chd = 0.02*(TC-6)+0.022*(SBP-120)) |>
  mutate(w_noncvd_chd = w_noncvd_chd + ifelse(current_smoker == "smoker", 0.63, 0)) |>
  mutate(sage_chd = s0age_chd^exp(w_chd)) |>
  mutate(sage10_chd = s0age10_chd^exp(w_chd)) |>
  mutate(sage_noncvd_chd = s0age_noncvd_chd^exp(w_noncvd_chd)) |>
  mutate(sage10_noncvd_chd = s0age10_noncvd_chd^exp(w_noncvd_chd)) |>
  mutate(s10age_chd = sage10_chd/sage_chd) |>
  mutate(s10age_noncvd_chd = sage10_noncvd_chd/sage_noncvd_chd) |>
  mutate(risk10_chd = 1-s10age_chd) |>
  mutate(risk10_noncvd_chd = 1-s10age_noncvd_chd) |>
  mutate(SCORE_Risk = risk10_chd+risk10_noncvd_chd) |>
  select(-alpha_chd, -p_chd, -s0age_chd, -s0age10_chd,
         -alpha_noncvd_chd, -p_noncvd_chd, -s0age_noncvd_chd, -s0age10_noncvd_chd,
         -w_chd, -w_noncvd_chd, -sage_chd, -sage10_chd,
         -sage_noncvd_chd, -sage10_noncvd_chd, -s10age_chd, -s10age_noncvd_chd,
         -risk10_chd, -risk10_noncvd_chd)
  
  

