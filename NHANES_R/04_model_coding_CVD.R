# Load needed libraries
library(tidyverse)
library(survey)

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

# initialize empty list
RESULTS_list <- list()

# loop for all survey nr cohorts
for (i in 1:length(surveys)) {
  
  # specify number of imputed datasets
  M <- 15
  ethnic_group <- c("All","Black","White","Hispanic")
  RESULTS <- data.frame(array(dim=c(3*length(ethnic_group),2)))
  colnames(RESULTS) <- c("pooled.avg","pooled.se")
  RESULTS$Ethnicity <- rep(c("All","Black","White","Hispanic"),3)
  RESULTS$Model <- c(rep(c("PCE"),length(ethnic_group)),
                     rep(c("Framingham"),length(ethnic_group)),
                     rep(c("SCORE"),length(ethnic_group)))
  
  for (ethn in ethnic_group) {
    
    # create an table data frames for result collection
    MI.PCE <- data.frame(array(dim=c(M,2)))
    rownames(MI.PCE) <- 1:M
    colnames(MI.PCE) <- c("avg","se")
    MI.Framingham <- data.frame(array(dim=c(M,2)))
    rownames(MI.Framingham) <- 1:M
    colnames(MI.Framingham) <- c("avg","se")
    MI.SCORE <- data.frame(array(dim=c(M,2)))
    rownames(MI.SCORE) <- 1:M
    colnames(MI.SCORE) <- c("avg","se")

    
    for (m in 1:M) {
      
      # load data
      imp1 <- readRDS(paste0("imputed_",surveys[i],"_", m, ".rds"))
      
      # calculate risk model variables
      ##########################
      ##### PCE RISK SCORE #####
      ##########################
      # The model is different for Whites and African Americans
      # We can now proceed towards writing the model for each ethnicity defined
      
      imp1 <- imp1 |> 
        mutate(Treated_Systolic_BP = ifelse(now_BP_meds == 'BP meds', SBP, 1)) |> 
        mutate(Untreated_Systolic_BP = ifelse(now_BP_meds == 'no BP meds', SBP, 1)) |> 
        mutate(smoker = ifelse(current_smoker == 'smoker', 1,0)) |> 
        mutate(new_TC = 38.67 * TC, new_HDL = 38.67 * HDL) |> 
        mutate(Risk_Sum = case_when(gender == 'female' ~ -29.799 * log(age) + 4.884 * (log(age)^2) + 
                                      13.540 * log(new_TC) -3.114 * (log(age) * log(new_TC)) - 
                                      13.578 * log(new_HDL) + 3.149 * (log(age) * log(new_HDL)) + 
                                      2.019 *  log(Treated_Systolic_BP) + 1.957 * log(Untreated_Systolic_BP) +
                                      7.574 * (smoker) -
                                      1.665 * (log(age) * (smoker)) + 
                                      0.661 * (diabetic == 'diabetes'),
                                    gender == 'male' ~ 12.344 * log(age) + 
                                      11.853 * log(new_TC) -2.664 * (log(age) * log(new_TC)) - 
                                      7.990 *  log(new_HDL) + 1.769 * (log(age) * log(new_HDL)) + 
                                      1.797 *  log(Treated_Systolic_BP) + 1.764 * log(Untreated_Systolic_BP) +
                                      7.837 * (smoker) -
                                      1.795 * (log(age) * (smoker)) + 
                                      0.658 * (diabetic == 'diabetes')),
               PCE_Risk_white = case_when(gender == 'female' ~ 1 - (0.9665)^exp(Risk_Sum + 29.18),
                                          gender == 'male'   ~ 1 - (0.9144)^exp(Risk_Sum - 61.18))) 
      
      
      imp1 <- imp1 |> 
        mutate(Treated_Systolic_BP = ifelse(now_BP_meds == 'BP meds', SBP, 1)) |> 
        mutate(Untreated_Systolic_BP = ifelse(now_BP_meds == 'no BP meds', SBP, 1)) |> 
        mutate(smoker = ifelse(current_smoker == 'smoker', 1,0)) |> 
        mutate(new_TC = 38.67 * TC, new_HDL = 38.67 * HDL) |>
        mutate(Risk_Sum = case_when(gender == 'female' ~ 17.114 * log(age) + 
                                      0.940 * log(new_TC) - 
                                      18.920 * log(new_HDL) + 4.475 * (log(age) * log(new_HDL)) + 
                                      29.291 *  log(Treated_Systolic_BP) -6.432 * (log(age) * log(Treated_Systolic_BP)) + 
                                      27.820 * log(Untreated_Systolic_BP) -6.087 * (log(age)* log(Untreated_Systolic_BP))+
                                      0.691 * (smoker) +
                                      0.874 * (diabetic == 'diabetes'),
                                    gender == 'male' ~ 2.469 * log(age) + 
                                      0.302 * log(new_TC) - 
                                      0.307 *  log(new_HDL) +
                                      1.916 *  log(Treated_Systolic_BP) + 1.809 * log(Untreated_Systolic_BP) +
                                      0.549 * (smoker) +
                                      0.645 * (diabetic == 'diabetes')),
               PCE_Risk_black = case_when(gender == 'female' ~ 1 - (0.9553)^exp(Risk_Sum - 86.61),
                                          gender == 'male'   ~ 1 - (0.8954)^exp(Risk_Sum - 19.54))) 
      
      
      imp1$Risk_PCE <- 0
      # for now, score for Whites are assigned to all participants who are not Black
      imp1$Risk_PCE[imp1$ethnicity != "Black"] <- imp1$PCE_Risk_white[imp1$ethnicity != "Black"] 
      imp1$Risk_PCE[imp1$ethnicity == "Black"] <- imp1$PCE_Risk_black[imp1$ethnicity == "Black"] 
      
      imp1$PCE_Risk_white <- NULL
      imp1$PCE_Risk_black <- NULL
      
      
      
      #################################
      ##### FRAMINGHAM RISK SCORE #####
      #################################
      # HDL mg/dL to mmol/L = #0.0259
      # TC mg/dL to mmol/L = #0.0259
      
      ## women
      imp1 <- imp1 |> 
        mutate(Framingham = 0)|>
        mutate(Framingham = Framingham + ifelse(age >= 35 & age < 40, 2,0))|>
        mutate(Framingham = Framingham + ifelse(age >=40 & age < 45,4,0))|>  
        mutate(Framingham = Framingham + ifelse(age >=45 & age < 50,5,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=50 & age < 55,7,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=55 & age < 60,8,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=60 & age < 65,9,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=65 & age < 70,10,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=70 & age < 75,11,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=75,12,0))|> 
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
        mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 120 & SBP < 130, 2,0))|>
        mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 130 & SBP < 149, 3,0))|>
        mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 140 & SBP < 150, 5,0))|>
        mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 150 & SBP < 160, 6,0))|>
        mutate(Framingham = Framingham + ifelse(now_BP_meds=="BP meds" & SBP >= 160, 7,0))|>
        mutate(Framingham = Framingham + ifelse(current_smoker=="smoker" , 3,0))|>
        mutate(Framingham = Framingham + ifelse(diabetic=="diabetes" , 4,0))|>
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
        mutate(Risk_Framingham_women = Risk_Framingham * 0.01) |>
        select(-Framingham, -Risk_Framingham)
      
      
      ## men
      imp1 <- imp1 |> 
        mutate(Framingham = 0)|>
        mutate(Framingham = Framingham + ifelse(age >= 35 & age < 40, 2,0))|>
        mutate(Framingham = Framingham + ifelse(age >=40 & age < 45,5,0))|>  
        mutate(Framingham = Framingham + ifelse(age >=45 & age< 50,6,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=50 & age <55,8,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=55 & age < 60,10,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=60 & age < 65,11,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=65 & age < 70,12,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=70 & age < 75,14,0))|> 
        mutate(Framingham = Framingham + ifelse(age >=75,15,0))|> 
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
        mutate(Risk_Framingham_men = Risk_Framingham * 0.01) |>
        select(-Framingham, -Risk_Framingham)
      
      imp1$Risk_Framingham <- 0
      imp1$Risk_Framingham[imp1$gender == "male"] <- imp1$Risk_Framingham_men[imp1$gender == "male"] 
      imp1$Risk_Framingham[imp1$gender == "female"] <- imp1$Risk_Framingham_women[imp1$gender == "female"] 
      
      imp1$Risk_Framingham_men <- NULL
      imp1$Risk_Framingham_women <- NULL
      
      
      #######################
      ##### SCORE SCORE #####
      #######################
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
        mutate(Risk_SCORE = risk10_chd+risk10_noncvd_chd) |>
        select(-alpha_chd, -p_chd, -s0age_chd, -s0age10_chd,
               -alpha_noncvd_chd, -p_noncvd_chd, -s0age_noncvd_chd, -s0age10_noncvd_chd,
               -w_chd, -w_noncvd_chd, -sage_chd, -sage10_chd,
               -sage_noncvd_chd, -sage10_noncvd_chd, -s10age_chd, -s10age_noncvd_chd,
               -risk10_chd, -risk10_noncvd_chd)
      
      
      imp1 <- imp1 |>
        select(survey_weight, SDMVPSU, SDMVSTRA, survey_nr, gender, age, ethnicity,
               diabetic, Risk_PCE, Risk_Framingham, Risk_SCORE)
      
      # survey design
      nhanes.y <- svydesign(data=imp1, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)
      
      # subsetting data
      # MODIFY: add line for ALL (if else loop as in T2D script)
      sub.y <- subset(nhanes.y, ethnicity == ethn & imp1$age>=40)
        
      # calculation of average predicted probabilities
      pred.y.PCE <- svymean(~Risk_PCE, sub.y)
      pred.y.Framingham <- svymean(~Risk_Framingham, sub.y)
      pred.y.SCORE <- svymean(~Risk_SCORE, sub.y)

      # the output svystat object is weird and needs to be coerced 
      # in a data frame before extracting the relevant stats
      MI.PCE[m,"avg"] <- as.numeric(as.data.frame(pred.y.PCE)[1])
      MI.PCE[m,"se"] <- as.numeric(as.data.frame(pred.y.PCE)[2])
      MI.Framingham[m,"avg"] <- as.numeric(as.data.frame(pred.y.Framingham)[1])
      MI.Framingham[m,"se"] <- as.numeric(as.data.frame(pred.y.Framingham)[2])
      MI.SCORE[m,"avg"] <- as.numeric(as.data.frame(pred.y.SCORE)[1])
      MI.SCORE[m,"se"] <- as.numeric(as.data.frame(pred.y.SCORE)[2])

    }
    
    # Rubin's rules, and assign to result collection df
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "PCE","pooled.avg"] <- rubin_mean(average = MI.PCE$avg)
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "PCE","pooled.se"] <- rubin_se(average = MI.PCE$avg, standard_error = MI.PCE$se)
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Framingham","pooled.avg"] <- rubin_mean(average = MI.Framingham$avg)
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Framingham","pooled.se"] <- rubin_se(average = MI.Framingham$avg, standard_error = MI.Framingham$se)
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "SCORE","pooled.avg"] <- rubin_mean(average = MI.SCORE$avg)
    RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "SCORE","pooled.se"] <- rubin_se(average = MI.SCORE$avg, standard_error = MI.SCORE$se)

  }
  
  RESULTS_list[[i]] <- RESULTS
  
}

# MODIFY: check what is wrong with Framingham!
# rbinding results from each survey year
RESULTS_df <- do.call("rbind", RESULTS_list)

# add baseline years
L <- nrow(RESULTS)
year_vals <- c(rep(1999,L),rep(2001,L),rep(2003,L),rep(2005,L),rep(2007,L),rep(2009,L),rep(2011,L),rep(2013,L),rep(2015,L),rep(2017,L))

RESULTS_df <- as.data.frame(cbind(RESULTS_df,
                                  baseline_year = year_vals))

# add model follow up times
RESULTS_df$year <- NA
RESULTS_df$year <- RESULTS_df$baseline_year + 10

# save results
saveRDS(RESULTS_df, "RESULTS_df_CVD.rds")







#############################
##### PCE (ASCVD) SCORE #####
#############################



#################################
##### FRAMINGHAM RISK SCORE #####
#################################




#################################
##### SCORE RISK ESTIMATION #####
#################################

# age needs to be more than 20
imp1 <- imp1[imp1$age > 20,]






