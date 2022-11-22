# Load needed libraries
library(tidyverse)
library(survey)
library(patchwork)

# Set working directory & load data
setwd("~/GitHub/NHANES_T2D/Data")

# Create a small function to return probabilities from logits (coefficients)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Create Rubin's Rules functions
# Rubin's Rules - pooling means
rubin_mean <- function(average) {
  Reduce("+", average) / length(average)
}

# Rubin's Rules - pooling SEs
rubin_se <- function(average, standard_error) {
  # Within imputation variance:
  within_var <- Reduce("+", lapply(standard_error, function(i){i*i})) / length(standard_error)
  # Between imputation variance:
  between_var <- Reduce("+", lapply(average, function(i){(i-rubin_mean(average))*(i-rubin_mean(average))})) / (length(average)-1)
  between_var2 <- between_var/ length(average)
  # Total variance:
  total_var <- within_var+between_var+between_var2
  # Pooled SE:
  sqrt(total_var)
}


# extract survey years
cleaned_full_df <- readRDS("cleaned_full_df.rds")
surveys <- levels(cleaned_full_df$survey_nr)


# specify number of imputed datasets
M <- 15

imps_data <- list()

  for (m in 1:M) {
    
    svy_data <- list()
    
    # loop for all survey nr cohorts
    for (i in 1:length(surveys)) {
      
      # load data
      imp1 <- readRDS(paste0("imputed_",surveys[i],"_", m, ".rds"))
      
      # calculate risk model variables
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
        mutate(Risk_Framingham = Risk_Framingham * 0.01)
      
      
      ####################################
      ####### National Screening #########
      ####################################
      
      imp1 <- imp1 |> 
        mutate(waist_inches = waist * 0.393701) |> #Convert into inches
        mutate(BMI_Categories = case_when(BMI >= 40 | (gender == 'male' & waist_inches >= 50) | (gender == 'female' & waist_inches >= 49) ~ 'Extremely Obese',
                                          (BMI >= 30 & BMI < 40)| (gender == 'male' & waist_inches >=40 & waist_inches < 50) | (gender == 'female' & waist_inches >= 35 & waist_inches < 49) ~ 'Obese',
                                          (BMI >= 25 & BMI < 30)| (gender == 'male' & waist_inches >=37 & waist_inches < 40) | (gender == 'female' & waist_inches >= 31.5 & waist_inches < 35) ~ 'Overweight',
                                          TRUE ~ 'Not Overweight or Obese'),
               BMI_Categories = factor(BMI_Categories,levels = c('Extremely Obese','Obese','Overweight','Not Overweight or Obese'))) |> 
        mutate(National_Screening = case_when((age < 40) ~ 0,
                                              (age >= 40 & age <= 49) ~ 1,
                                              (age >= 50 & age <= 59) ~ 2,
                                              age >= 60 ~ 3)) |> 
        mutate(National_Screening = National_Screening + if_else(gender == 'male', 1,0)) |> 
        mutate(National_Screening = National_Screening + if_else(famhist_T2D == 'family diabetes', 1, 0)) |> 
        mutate(National_Screening = National_Screening + if_else((hypertension_now == 'hypertension' | now_BP_meds == 'BP meds'), 1, 0)) |> 
        mutate(National_Screening = National_Screening + case_when(BMI_Categories == 'Extremely Obese' ~ 3,
                                                                   BMI_Categories == 'Obese' ~ 2,
                                                                   BMI_Categories == 'Overweight' ~ 1,
                                                                   BMI_Categories == 'Not Overweight or Obese' ~ 0)) |> 
        mutate(National_Screening = National_Screening + if_else(active == 'Yes', -1, 0)) |> 
        mutate(NS_Score = if_else(National_Screening >= 4, 1, 0))
      
      
      
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
        mutate(Risk_ARIC = logit2prob(ARIC))
      

      imp1 <- imp1 |>
        select(survey_weight, SDMVPSU, SDMVSTRA, survey_nr, gender, age, ethnicity,
               diabetic, Framingham, Risk_Framingham, National_Screening,
               ARIC, Risk_ARIC, NS_Score)
      
      # survey design
      nhanes.y <- svydesign(data=imp1, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
      
      # subsetting data

      sub.y <- subset(nhanes.y, (imp1$ethnicity == "Black" | imp1$ethnicity == "White") & imp1$age>=18 & imp1$diabetic == 'no diabetes') 

      svy_data[[i]] <- sub.y$variables
      
    }
    binded_svy_data <- do.call("rbind", svy_data)
    imps_data[[m]] <- binded_svy_data
  }


########################################
########################################
############### PLOTTING ###############
########################################
########################################


# directory to Plots
setwd("~/GitHub/NHANES_T2D/Plots")

# Framingam: this is the score
# Risk_Framingham: converted risk
# National_Screening: this is the score
# NS_Score: converted 1/0 for screening
# ARIC: this is the score
# Risk_ARIC: converted 

df <- imps_data[[1]]

cor(df$Risk_Framingham, df$National_Screening)
cor(df$Risk_ARIC, df$National_Screening)


df$National_Screening <- as.factor(df$National_Screening)
df$Ethnicity <- df$ethnicity
df$ethnicity <- NULL
df$Ethnicity <- as.factor(df$Ethnicity)
df$Ethnicity <- droplevels(df$Ethnicity)
levels(df$Ethnicity)[1] <- 'Non-Hispanic Whites'
levels(df$Ethnicity)[2] <- 'Non-Hispanic Blacks'


p1 <- ggplot(data = df, aes(x=Risk_Framingham, y=National_Screening)) +
  geom_boxplot(aes(col = Ethnicity), outlier.shape = NA, lwd=1.25) +
  xlab("Framingham predicted risk") +
  ylab("Prediabetes Risk Score") +
  theme_minimal() +
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        strip.text = element_text(size=rel(3)),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  geom_hline(yintercept = 6.5, lwd = 1.25, linetype = "dotted", color = "#7570B3") +
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))


p2 <- ggplot(data = df, aes(x=Risk_ARIC, y=National_Screening)) +
  geom_boxplot(aes(col = Ethnicity), outlier.shape = NA, lwd=1.25) +
  xlab("ARIC predicted risk") +
  ylab("Prediabetes Risk Score") +
  theme_minimal() +
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        strip.text = element_text(size=rel(3)),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  geom_hline(yintercept = 6.5, lwd = 1.25, linetype = "dotted", color = "#7570B3") +
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("NS_boxplots.png", width = 1200, height = 1200)
p1/p2
dev.off()








