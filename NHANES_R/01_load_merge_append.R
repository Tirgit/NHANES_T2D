# LOAD LIBRARIES
# library(haven)
library(foreign)
library(dplyr)

# SET WORKING DIRECTORY
setwd("H:/BACKUP/Projects/NHANES")

# LOAD AND MERGE ALL NECESSARY FILES PER SURVEY

######################################
########## NHANES 1999-2000 ##########
######################################

## DEMOGRAPHICS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/DEMO.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1", "INDHHINC", "DMDEDUC",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BMX.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BPX.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSAR", "BPXDAR", "BPXPLS")
bloodp <- loaded_file[,keep_vars]

## BODY COMPOSITION
# download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BIX.XPT", tf <- tempfile(), mode="wb")
# loaded_file <- foreign::read.xport(tf)
# keep_vars <- c("SEQN", "BIDPFAT")
# bodycomp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/LAB13AM.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/LAB13.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI", "LBDHDLSI")
lipid_2 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BPQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/LAB18.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSUASI", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/MCQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ250A", "MCQ260AA", "MCQ260AB",
               "MCQ260AG", "MCQ260AH", "MCQ080",
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F", "MCQ250F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/SMQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/PH.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
smoking <- loaded_file[,keep_vars]

## PHYSICAL ACTIVITY
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/PAQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PAQ180", "PAD200", "PAD320",
               "PAD440", "PAD460")
activity <- loaded_file[,keep_vars]

## DIABETES
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/DIQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070",
               "DIQ080", "DIQ090", "DIQ100", "DIQ120",
               "DIQ140", "DIQ150")
diabetes <- loaded_file[,keep_vars]


## PRESCRIPTION MEDS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/RXQ_RX.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RXD240B")
meds <- loaded_file[,keep_vars]
# DO NOT MERGE! CLEAN PRESCRIPTIONS FIRST!






## MERGE ALL BY SEQN
full_1999_2000 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(activity,  by = "SEQN") %>%
  full_join(diabetes,  by = "SEQN")

## RENAME VARIABLES
colnames(full_1999_2000) <- c("SEQN", "gender", "age", "ethnicity", 
                              "income", "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP", "DBP", "pulse",
                              "TG", "LDL", "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds","uric_acid", "glucose",
                              "famhist_T2D", "famhist_T2D_mother", "famhist_T2D_father",
                              "famhist_T2D_brother", "famhist_T2D_sister",
                              "ever_overweight", "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattach",
                              "ever_stroke", "famhist_hypertension",
                              "current_smoker", "fasting_hr",
                              "PA_work", "PA_vigorous", "PA_moderate",
                              "PA_strength", "PA_freq",
                              "ever_diabetes", "insulin", "oral_diab_med",
                              "comp_retinopathy", "comp_ulcer",
                              "comp_numbness", "comp_pain_hand",
                              "comp_pain_leg", "comp_pain_calf")







######################################
########## NHANES 2017-2018 ##########
######################################

## DEMOGRAPHICS
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1")
demo <- loaded_file[,keep_vars]
colnames(demo) <- c("SEQN", "gender", "age", "ethnicity")






