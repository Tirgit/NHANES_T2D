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

# For imputation 1


imp1 <- plyr::ddply(imp1,.variables = 'survey_nr', transform,
              Predictions_Fram = predict(glm(diabetic ~ gender + glucose + BMI + HDL + famhist_T2D + 
                                             TG + SBP + DBP + now_BP_meds,
                                             family = binomial(link = 'logit')),type = 'response'))

imp1$Predictions_Fram <- round(imp1$Predictions_Fram,4)


# For imputation 2

imp2 <- plyr::ddply(imp2, .variables = 'survey_nr', transform,
              Predictions_Fram = predict(glm(diabetic ~ gender + glucose + BMI + HDL + famhist_T2D + 
                                             TG + SBP + DBP + now_BP_meds,
                                             family = binomial(link = 'logit')),type = 'response'))

imp2$Predictions_Fram <- round(imp2$Predictions_Fram,4)


# For imputation 3

imp3 <- plyr::ddply(imp3,.variables = 'survey_nr', transform,
              Predictions_Fram = predict(glm(diabetic ~ gender + glucose + BMI + HDL + famhist_T2D + 
                                             TG + SBP + DBP + now_BP_meds,
                                             family = binomial(link = 'logit')),type = 'response'))

imp3$Predictions_Fram <- round(imp3$Predictions_Fram,4)


# For imputation 4

imp4 <- plyr::ddply(imp4,.variables = 'survey_nr', transform,
              Predictions_Fram = predict(glm(diabetic ~ gender + glucose + BMI + HDL + famhist_T2D + 
                                             TG + SBP + DBP + now_BP_meds,
                                             family = binomial(link = 'logit')),type = 'response'))

imp4$Predictions_Fram <- round(imp4$Predictions_Fram,4)


# For imputation 5

imp5 <- plyr::ddply(imp5, .variables = 'survey_nr', transform,
              Predictions_Fram = predict(glm(diabetic ~ gender + glucose + BMI + HDL + famhist_T2D + 
                                             TG + SBP + DBP + now_BP_meds,
                                             family = binomial(link = 'logit')),type = 'response'))

imp5$Predictions_Fram <- round(imp5$Predictions_Fram,4)


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


# We can plot the predictions against specific key variables of interest stratified on ethnicity

# First create some percentiles for risk predictions (this takes into account all the survey years to create 
# the percentiles)

imp1 <- imp1 |> 
  mutate(Risk_Percentiles_Fram = cut2(Predictions_Fram, g = 20, levels.mean = T)) 

# We create 20 percentiles, we can do more or less, g is defining the number


# Now we can create plots with risk percentiles on the x-axis and key variables on the y-axis
# Same as the figures in the paper Tibor suggested to read before our workshop (We might need weights for the
# above)

p1_99 <- imp1 |> 
  group_by(Risk_Percentiles_Fram, ethnicity) |>
  summarise(Mean_HDL = mean(HDL)) |> 
  mutate(Percentiles = as.numeric(Risk_Percentiles_Fram)) |> 
  ggplot(aes(x = Percentiles, y = Mean_HDL, col = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_smooth(aes(col = ethnicity), se = F, linetype = 'solid') +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Percentiles', y = 'Mean HDL', title = 'Mean HDL by risk percentile stratified on ethnicity') +
  geom_vline(xintercept = 17, linetype = 'dashed',col = 'hotpink3')

p2_99 <- imp1 |> 
  group_by(Risk_Percentiles_Fram, ethnicity) |>
  summarise(Mean_Glucose = mean(glucose)) |> 
  mutate(Percentiles = as.numeric(Risk_Percentiles_Fram)) |> 
  ggplot(aes(x = Percentiles, y = Mean_Glucose, col = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_smooth(aes(col = ethnicity), se = F, linetype = 'solid') +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Percentiles', y = 'Mean Glucose', title = 'Mean Glucose by risk percentile stratified on ethnicity') +
  geom_vline(xintercept = 17, linetype = 'dashed',col = 'hotpink3')


p3_99 <- imp1 |> 
  group_by(Risk_Percentiles_Fram, ethnicity) |>
  summarise(Mean_TG = mean(TG)) |> 
  mutate(Percentiles = as.numeric(Risk_Percentiles_Fram)) |> 
  ggplot(aes(x = Percentiles, y = Mean_TG, col = ethnicity)) +
  geom_point2(stroke = 1.4) +
  geom_smooth(aes(col = ethnicity), se = F, linetype = 'solid') +
  theme_ipsum_rc() +
  ggokabeito::scale_color_okabe_ito() +
  theme(legend.position = 'top') +
  labs(x = 'Risk Percentiles', y = 'Mean TG', title = 'Mean TG by risk percentile stratified on ethnicity') +
  geom_vline(xintercept = 17, linetype = 'dashed',col = 'hotpink3')


(p1_99 + p2_99 + p3_99)


##############################################
############ DESIR Score #####################
##############################################

# The DESIR Score has the following predictors :

# Sex + Waist Circumference + Hypertension + Smoking Status + Family History of Diabetes


# For imputation 1


imp1 <- plyr::ddply(imp1,.variables = 'survey_nr', transform,
                    Predictions_Desir = predict(glm(diabetic ~ gender + waist + hypertension_now + 
                                                    current_smoker + famhist_T2D,
                                                    family = binomial(link = 'logit')),type = 'response'))

imp1$Predictions_Desir <- round(imp1$Predictions_Desir,4)



# For imputation 2


imp2 <- plyr::ddply(imp2,.variables = 'survey_nr', transform,
                    Predictions_Desir = predict(glm(diabetic ~ gender + waist + hypertension_now + 
                                                      current_smoker + famhist_T2D,
                                                    family = binomial(link = 'logit')),type = 'response'))

imp2$Predictions_Desir <- round(imp2$Predictions_Desir,4)


# For imputation 3


imp3 <- plyr::ddply(imp3,.variables = 'survey_nr', transform,
                    Predictions_Desir = predict(glm(diabetic ~ gender + waist + hypertension_now + 
                                                      current_smoker + famhist_T2D,
                                                    family = binomial(link = 'logit')),type = 'response'))

imp3$Predictions_Desir <- round(imp3$Predictions_Desir,4)


# For imputation 4


imp4 <- plyr::ddply(imp4,.variables = 'survey_nr', transform,
                    Predictions_Desir = predict(glm(diabetic ~ gender + waist + hypertension_now + 
                                                      current_smoker + famhist_T2D,
                                                    family = binomial(link = 'logit')),type = 'response'))

imp4$Predictions_Desir <- round(imp4$Predictions_Desir,4)


# For imputation 5


imp5 <- plyr::ddply(imp5,.variables = 'survey_nr', transform,
                    Predictions_Desir = predict(glm(diabetic ~ gender + waist + hypertension_now + 
                                                      current_smoker + famhist_T2D,
                                                    family = binomial(link = 'logit')),type = 'response'))

imp5$Predictions_Desir <- round(imp5$Predictions_Desir,4)


# Create some percentiles for risk predictions (this takes into account all the survey years to create 
# the percentiles)

imp1 <- imp1 |> 
  mutate(Risk_Percentiles_Desir = cut2(Predictions_Desir, g = 20, levels.mean = T)) 


# At that stage every imp dataset has predictions per survey number both for Framingham and Desir