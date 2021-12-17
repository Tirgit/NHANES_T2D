Sys.setenv(LANG = "en")
# Set working directory
setwd("~/GitHub/NHANES_T2D/NHANES_R")

####################################
###### Framingham Offspring ########
####################################


# The Framingham score uses the following predictors : 
# Sex, Fasting Plasma Glucose, BMI, High Density Lipoprotein Cholesterol level, Parental History of Diabetes, 
# Triglyceride level, Systolic and Diastolic blood pressure, Antihypertensive treatment


# Open the imputed  data frames

imp1 <- readRDS('imputed_df_1.rds')
imp2 <- readRDS('imputed_df_2.rds')
imp3 <- readRDS('imputed_df_3.rds')
imp4 <- readRDS('imputed_df_4.rds')
imp5 <- readRDS('imputed_df_5.rds')


# Let's create a logistic regression model to get risk probabilities of diabetes.
# We can do this for each imputation separately but looped for each level 
# of the survey number variable for speed.

# Let's load some packages

library(tidyverse)
library(broom)
library(easystats)
library(ggthemes)
library(hrbrthemes)
library(ggokabeito)
library(Hmisc)
library(janitor)
library(riskRegression)
library(patchwork)  


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
  mutate(Framingham = Framingham + ifelse(BMI >= 25 & BMI <30, 2,0)) |> 
  mutate(Framingham = Framingham + ifelse(BMI >= 30, 5, 0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'male' & HDL < 1.036, 5,0)) |> 
  mutate(Framingham = Framingham + ifelse(gender == 'female' & HDL < 1.295, 5,0)) |> 
  mutate(Framingham = Framingham + ifelse(famhist_T2D == 'family diabetes', 3,0)) |> 
  mutate(Framingham = Framingham + ifelse(TG >= 1.695, 3, 0 )) |> 
  mutate(Framingham = Framingham + ifelse(SBP >= 130 | DBP >= 85 | now_BP_meds == 'BP meds', 2,0)) |> 
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
         

# Let's just plot the ethnicity percentages across the survey years

imp1 |> 
  group_by(survey_nr) |> 
  count(ethnicity) |> 
  mutate(percent = n / sum(n)) |>  
  ggplot(aes(x = ethnicity, y = percent, fill = ethnicity)) + 
  geom_col() +
  scale_fill_okabe_ito() +
  theme_ipsum() +
  facet_wrap(vars(survey_nr),scales = 'free',ncol = 5) +
  theme(legend.position = 'top') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = NULL) +
  labs(x = NULL, title = 'Ethnicity percentages stratified survey number')




#############################################################################################################
############### Some analysis afterwards, could be followed for the other imputations, risk models 
####################################### and survey numbers ##################################################
#############################################################################################################



# Now we can create plots with risk percentiles on the x-axis and key variables on the y-axis
# Same as the figures in the paper Tibor suggested to read before our workshop (We might need weights for the
# above)

p1 <- imp1 |> 
  group_by(Risk_Framingham, ethnicity) |>
  summarise(Mean_HDL = mean(HDL)) |>
  ggplot(aes(x = Risk_Framingham, y = Mean_HDL, col = ethnicity, group = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_line(aes(group = ethnicity)) +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Probability', y = 'Mean HDL', title = 'Mean HDL by risk percentile stratified on ethnicity')

p2 <- imp1 |> 
  group_by(Risk_Framingham, ethnicity) |>
  summarise(Mean_Glucose = mean(glucose)) |> 
  ggplot(aes(x = Risk_Framingham, y = Mean_Glucose, col = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_line(aes(group = ethnicity)) +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Probability', y = 'Mean Glucose', title = 'Mean Glucose by risk percentile stratified on ethnicity') 


p3 <- imp1 |> 
  group_by(Risk_Framingham, ethnicity) |>
  summarise(Mean_TG = mean(TG)) |> 
  ggplot(aes(x = Risk_Framingham, y = Mean_TG, col = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_line(aes(group = ethnicity)) +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Probabilities', y = 'Mean TG', title = 'Mean TG by risk percentile stratified on ethnicity') 


(p1 + p2 + p3)


