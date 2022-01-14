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
# HDL mg/dL to mmol/L = #0.0259
# TC mg/dL to mmol/L = #0.0259

## women
imp1 <- imp1 |> 
  mutate(Framingham = 0)|>
  mutate(Framingham= Framingham + ifelse(age >= 35 & age < 40, 2,0))|>
  mutate(Framingham= Framingham + ifelse(age >=40 & age < 45,4,0))|>  
  mutate(Framingham= Framingham + ifelse(age >=45 & age < 50,5,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=50 & age < 55,7,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=55 & age < 60,8,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=60 & age < 65,9,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=65 & age < 70,10,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=70 & age < 75,11,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=75,12,0))|> 
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*60), -2,0)) |>
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*50) & HDL < (0.0259*60), -1,0)) |>
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*35) & HDL < (0.0259*45), 1,0))|> 
  mutate(Framingham = Framingham + ifelse(HDL < (0.0259*35),2,0)) |>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*160) & TC < (0.0259*200),1,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*200) & TC < (0.0259*240),3,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*240) & TC < (0.0259*280),4,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*280),5,0))|> 
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP < 120, -3,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 130 & SBP <140, 1,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 140 & SBP <150, 2,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 150 & SBP < 160, 4,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 160, 5,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP < 120, -1,0))|>
  mutate(Framingham = Framingham + ifelse(ow_BP_meds=="BP meds" & SBP >= 120 & SBP < 130, 2,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 130 & SBP < 149, 3,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 140 & SBP < 150, 5,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 150 & SBP < 160, 6,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 160, 7,0))|>
  mutate(Framingham = Framingham + ifelse(current_smoker=="smoker" , 3,0))|>
  mutate(Framingham = Framingham + ifelse(diabetic=="diabetes" , 4,0))|>
  mutate(Risk_Framingham = case_when(Framingham <= -3 ~ 0,
                                     Framingham == -2 ~ 1.1,
                                     Framingham == -1 ~ 1.4,
                                     Framingham == 0 ~ 1.6,
                                     Framingham == 1 ~ 1.9,
                                     Framingham == 2 ~ 2.3,
                                     Framingham == 3 ~ 2.8,
                                     Framingham == 4 ~ 3.3,
                                     Framingham == 5 ~ 3.9,
                                     Framingham == 6 ~ 4.7,
                                     Framingham == 7 ~ 5.6,
                                     Framingham == 8 ~ 6.7,
                                     Framingham == 9 ~ 7.9,
                                     Framingham == 10 ~ 9.4,
                                     Framingham == 11 ~ 11.2,
                                     Framingham == 12 ~ 13.2,
                                     Framingham == 13 ~ 15.6,
                                     Framingham == 14 ~ 18.4,
                                     Framingham == 15 ~ 21.6,
                                     Framingham == 16 ~ 25.3,
                                     Framingham == 17 ~ 29.4,
                                     Framingham == 18 ~ 30)) |> 
  mutate(Risk_Framingham = Risk_Framingham * 0.01) |>
  select(-Framingham)

## men
imp1 <- imp1 |> 
  mutate(Framingham = 0)|>
  mutate(Framingham= Framingham + ifelse(age >= 35 & age < 40, 2,0))|>
  mutate(Framingham= Framingham + ifelse(age >=40 & age < 45,5,0))|>  
  mutate(Framingham= Framingham + ifelse(age >=45 & age< 50,6,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=50 & age <55,8,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=55 & age < 60,10,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=60 & age < 65,11,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=65 & age < 70,12,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=70 & age < 75,14,0))|> 
  mutate(Framingham= Framingham + ifelse(age >=75,15,0))|> 
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*60), -2,0)) |>
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*50) & HDL < (0.0259*60), -1,0)) |>
  mutate(Framingham = Framingham + ifelse(HDL >= (0.0259*35) & HDL < (0.0259*45), 1,0))|> 
  mutate(Framingham = Framingham + ifelse(HDL < (0.0259*35),2,0)) |>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*160) & TC < (0.0259*200),1,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*200) & TC < (0.0259*240),2,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*240) & TC < (0.0259*280),3,0))|>
  mutate(Framingham = Framingham + ifelse(TC >= (0.0259*280),4,0))|> 
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP < 120, -2,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 130 & SBP < 140, 1,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 140 & SBP < 160, 2,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="no BP meds" & SBP >= 160, 3,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 120 & SBP < 130, 2,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 130 & SBP < 140, 3,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 140 & SBP < 160, 4,0))|>
  mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds"& SBP >= 160, 5,0))|>
  mutate(Framingham = Framingham + ifelse(current_smoker=="smoker" , 4,0))|>
  mutate(Framingham = Framingham + ifelse(diabetic=="diabetes" , 3,0))|>
  mutate(Risk_Framingham = case_when(Framingham <= -2 ~ 0,
                                     Framingham == -1 ~ 1.0,
                                     Framingham == 0 ~ 1.2,
                                     Framingham == 1 ~ 1.5,
                                     Framingham == 2 ~ 1.7,
                                     Framingham == 3 ~ 2.0,
                                     Framingham == 4 ~ 2.4,
                                     Framingham == 5 ~ 2.8,
                                     Framingham == 6 ~ 3.3,
                                     Framingham == 7 ~ 3.9,
                                     Framingham == 8 ~ 4.5,
                                     Framingham == 9 ~ 5.3,
                                     Framingham == 10 ~ 6.3,
                                     Framingham == 11 ~ 7.3,
                                     Framingham == 12 ~ 8.6,
                                     Framingham == 13 ~ 10.0,
                                     Framingham == 14 ~ 11.7,
                                     Framingham == 15 ~ 13.7,
                                     Framingham == 16 ~ 15.9,
                                     Framingham == 17 ~ 18.5,
                                     Framingham == 18 ~ 21.5,
                                     Framingham == 19 ~ 24.8,
                                     Framingham == 20 ~ 28.5,
                                     Framingham >= 21 ~ 30)) |> 
  mutate(Risk_Framingham = Risk_Framingham * 0.01) |>
  select(-Framingham)


###############################
##### REYNOLDS RISK SCORE #####
###############################

xxxxx

#################################
##### SCORE RISK ESTIMATION #####
#################################

xxxxx





