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


# specify number of imputed datasets
M <- 15
ethnic_group <- c("All","Black","White","Hispanic")
RESULTS <- data.frame(array(dim=c(5*length(ethnic_group),2)))
colnames(RESULTS) <- c("pooled.avg","pooled.se")
RESULTS$Ethnicity <- rep(c("All","Black","White","Hispanic"),5)
RESULTS$Model <- c(rep(c("Framingham"),length(ethnic_group)),
                   rep(c("DESIR"),length(ethnic_group)),
                   rep(c("EGATS"),length(ethnic_group)),
                   rep(c("ARIC"),length(ethnic_group)),
                   rep(c("Antonio"),length(ethnic_group)))

for (ethn in ethnic_group) {
  
  # create an table data frames for result collection
  MI.Framingham <- data.frame(array(dim=c(M,2)))
  rownames(MI.Framingham) <- 1:M
  colnames(MI.Framingham) <- c("avg","se")
  MI.DESIR <- data.frame(array(dim=c(M,2)))
  rownames(MI.DESIR) <- 1:M
  colnames(MI.DESIR) <- c("avg","se")
  MI.EGATS <- data.frame(array(dim=c(M,2)))
  rownames(MI.EGATS) <- 1:M
  colnames(MI.EGATS) <- c("avg","se")
  MI.ARIC <- data.frame(array(dim=c(M,2)))
  rownames(MI.ARIC) <- 1:M
  colnames(MI.ARIC) <- c("avg","se")
  MI.Antonio <- data.frame(array(dim=c(M,2)))
  rownames(MI.Antonio) <- 1:M
  colnames(MI.Antonio) <- c("avg","se")
  
  for (m in 1:M) {
    
    # load data
    imp1 <- readRDS(paste0("imputed_1999-2000_", m, ".rds"))
    
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
      mutate(Risk_EGATS = ifelse(EGATS > 6, 1, 0)) |>
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
      select(survey_weight, SDMVPSU, SDMVSTRA, survey_nr, gender, age, ethnicity,
             diabetic, Risk_Framingham, Risk_DESIR, Risk_EGATS,
             Risk_ARIC, Risk_Antonio)
    
    # survey design
    nhanes.y <- svydesign(data=imp1, id=~SDMVPSU, strata=~SDMVSTRA, weights=~survey_weight, nest=TRUE)

    # subsetting data
    if (ethn == "All") {
    sub.y <- subset(nhanes.y, imp1$age>=18 & imp1$diabetic == 'no diabetes') 
    } else {
      sub.y <- subset(nhanes.y, ethnicity == ethn & imp1$age>=18 & imp1$diabetic == 'no diabetes') 
    }
    
    # calculation of average predicted probabilities
    pred.y.Framingham <- svymean(~Risk_Framingham, sub.y)
    pred.y.DESIR <- svymean(~Risk_DESIR, sub.y)
    pred.y.EGATS <- svymean(~Risk_EGATS, sub.y)
    pred.y.ARIC <- svymean(~Risk_ARIC, sub.y)
    pred.y.Antonio <- svymean(~Risk_Antonio, sub.y)
    
    # the output svystat object is weird and needs to be coerced 
    # in a data frame before extracting the relevant stats
    MI.Framingham[m,"avg"] <- as.numeric(as.data.frame(pred.y.Framingham)[1])
    MI.Framingham[m,"se"] <- as.numeric(as.data.frame(pred.y.Framingham)[2])
    MI.DESIR[m,"avg"] <- as.numeric(as.data.frame(pred.y.DESIR)[1])
    MI.DESIR[m,"se"] <- as.numeric(as.data.frame(pred.y.DESIR)[2])
    MI.EGATS[m,"avg"] <- as.numeric(as.data.frame(pred.y.EGATS)[1])
    MI.EGATS[m,"se"] <- as.numeric(as.data.frame(pred.y.EGATS)[2])
    MI.ARIC[m,"avg"] <- as.numeric(as.data.frame(pred.y.ARIC)[1])
    MI.ARIC[m,"se"] <- as.numeric(as.data.frame(pred.y.ARIC)[2])
    MI.Antonio[m,"avg"] <- as.numeric(as.data.frame(pred.y.Antonio)[1])
    MI.Antonio[m,"se"] <- as.numeric(as.data.frame(pred.y.Antonio)[2])
    
  }
  
  # Rubin's rules, and assign to result collection df
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Framingham","pooled.avg"] <- rubin_mean(average = MI.Framingham$avg)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Framingham","pooled.se"] <- rubin_se(average = MI.Framingham$avg, standard_error = MI.Framingham$se)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "DESIR","pooled.avg"] <- rubin_mean(average = MI.DESIR$avg)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "DESIR","pooled.se"] <- rubin_se(average = MI.DESIR$avg, standard_error = MI.DESIR$se)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "EGATS","pooled.avg"] <- rubin_mean(average = MI.EGATS$avg)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "EGATS","pooled.se"] <- rubin_se(average = MI.EGATS$avg, standard_error = MI.EGATS$se)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "ARIC","pooled.avg"] <- rubin_mean(average = MI.ARIC$avg)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "ARIC","pooled.se"] <- rubin_se(average = MI.ARIC$avg, standard_error = MI.ARIC$se)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Antonio","pooled.avg"] <- rubin_mean(average = MI.Antonio$avg)
  RESULTS[RESULTS$Ethnicity == ethn & RESULTS$Model == "Antonio","pooled.se"] <- rubin_se(average = MI.Antonio$avg, standard_error = MI.Antonio$se)
  
}
    

### pick up from here and:
### - code this for all survey nr s!






# generate result dataframe
model_vals <- c(rep("Framingham",50),rep("DESIR",50),rep("EGATS",50),rep("ARIC",50),rep("San Antonio",50))
year_vals <- rep(c(rep(1999,5),rep(2001,5),rep(2003,5),rep(2005,5),rep(2007,5),rep(2009,5),rep(2011,5),rep(2013,5),rep(2015,5),rep(2017,5)),5)
ethnicity_vals <- rep(c("All", levels(imp1_excl$ethnicity)),50)
result_df <- as.data.frame(cbind(avg_pred = estimate_list,
                                 model = model_vals,
                                 baseline_year = year_vals,
                                 ethnicity = ethnicity_vals))
result_df$avg_pred <- as.numeric(result_df$avg_pred)
result_df$baseline_year <- as.numeric(result_df$baseline_year)

# add model follow up times
result_df$year <- NA
result_df$year[result_df$model == "Framingham" | result_df$model == "San Antonio"] <- result_df$baseline_year[result_df$model == "Framingham" | result_df$model == "San Antonio"] + 8
result_df$year[result_df$model == "DESIR" | result_df$model == "ARIC"] <- result_df$baseline_year[result_df$model == "DESIR" | result_df$model == "ARIC"] + 9
result_df$year[result_df$model == "EGATS"] <- result_df$baseline_year[result_df$model == "EGATS"] + 12

# drop Other ethnicity from results
result_df <- result_df[result_df$ethnicity != "Other",] 

saveRDS(result_df, "result_df.rds")





  