# Set working directory
setwd("~/GitHub/NHANES_T2D/Data")

# load necessary libraries
library(foreign)
library(dplyr)

# LOAD AND MERGE ALL NECESSARY FILES PER SURVEY

######################################
########## NHANES 1999-2000 ##########
######################################
years <- "1999-2000"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO.XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX.XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX.XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/LAB13AM.XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/LAB13.XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ.XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/LAB18.XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ.XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ.XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PH.XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ.XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/LAB10.XPT")

## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI", "LBDHDLSI")
lipid_2 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ250A", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


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
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")

## RENAME VARIABLES
colnames(full_1999_2000) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D", 
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke", 
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file)

######################################
########## NHANES 2001-2002 ##########
######################################
years <- "2001-2002"
letter <- "B"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L13AM_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L13_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L40_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PH_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L10_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI", "LBDHDLSI")
lipid_2 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ250A", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]



## MERGE ALL BY SEQN
full_2001_2002 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2001_2002) <-  c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D", 
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke", 
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file)


######################################
########## NHANES 2003-2004 ##########
######################################
years <- "2003-2004"
letter <- "C"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L13AM_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L13_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L40_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/PH_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/L10_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI", "LBDHDDSI")
lipid_2 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ250A", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2003_2004 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2003_2004) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D", 
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke", 
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file)

######################################
########## NHANES 2005-2006 ##########
######################################

years <- "2005-2006"
letter <- "D"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DID070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2005_2006 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2005_2006) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2007-2008 ##########
######################################

years <- "2007-2008"
letter <- "E"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN2")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DID070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2007_2008 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2007_2008) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2009-2010 ##########
######################################
years <- "2009-2010"
letter <- "F"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN2")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2009_2010 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2009_2010) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)


######################################
########## NHANES 2011-2012 ##########
######################################
years <- "2011-2012"
letter <- "G"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")

## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]

## MERGE ALL BY SEQN
full_2011_2012 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2011_2012) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2013-2014 ##########
######################################
years <- "2013-2014"
letter <- "H"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]

## MERGE ALL BY SEQN
full_2013_2014 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")

## RENAME VARIABLES
colnames(full_2013_2014) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2015-2016 ##########
######################################
years <- "2015-2016"
letter <- "I"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")
a1c_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/GHB_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2015_2016 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2015_2016) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)

######################################
########## NHANES 2017-2018 ##########
######################################
years <- "2017-2018"
letter <- "J"

demo_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DEMO_",letter,".XPT")
body_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BMX_",letter,".XPT")
bloodp_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPX_",letter,".XPT")
lipid_1_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TRIGLY_",letter,".XPT")
lipid_2_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/TCHOL_",letter,".XPT")
lipid_3_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/HDL_",letter,".XPT")
meds_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BPQ_",letter,".XPT")
standard_lab_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/BIOPRO_",letter,".XPT")
fam_hist_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/MCQ_",letter,".XPT")
smoking_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/SMQ_",letter,".XPT")
fasting_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/FASTQX_",letter,".XPT")
diabetes_file <- paste0("https://wwwn.cdc.gov/nchs/nhanes/",years,"/DIQ_",letter,".XPT")


## DEMOGRAPHICS
download.file(demo_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SDMVPSU", "SDMVSTRA", "SDDSRVYR",  "RIAGENDR", "RIDAGEYR", 
               "RIDRETH1",  "DMDEDUC2",
               "RIDEXPRG", "DMDBORN4")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file(body_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI", "BMXHT")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file(bloodp_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4",
               "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
bloodp <- loaded_file[,keep_vars]

## LIPIDS 1
download.file(lipid_1_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTRSI", "LBDLDLSI", "WTSAF2YR")
lipid_1 <- loaded_file[,keep_vars]

## LIPIDS 2
download.file(lipid_2_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDTCSI")
lipid_2 <- loaded_file[,keep_vars]

## LIPIDS 3
download.file(lipid_3_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDHDDSI")
lipid_3 <- loaded_file[,keep_vars]

## HYPERTENSION & LIPID MEDS
download.file(meds_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D",
               "BPQ050A")
meds <- loaded_file[,keep_vars]

## LABORATORY PANEL
download.file(standard_lab_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBDSGLSI")
standard_lab <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file(fam_hist_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ300C", 
               "MCQ160B", "MCQ160C", "MCQ160D",
               "MCQ160E", "MCQ160F")
fam_hist <- loaded_file[,keep_vars]

## SMOKING
download.file(smoking_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "SMQ040")
smoking <- loaded_file[,keep_vars]

## FASTING STATUS
download.file(fasting_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "PHAFSTHR")
fasting <- loaded_file[,keep_vars]

## DIABETES
download.file(diabetes_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070")
diabetes <- loaded_file[,keep_vars]

## HbA1c
download.file(a1c_file, tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "LBXGH")
a1c <- loaded_file[,keep_vars]


## MERGE ALL BY SEQN
full_2017_2018 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(lipid_3,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(standard_lab,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") %>% 
  full_join(smoking,  by = "SEQN") %>% 
  full_join(fasting,  by = "SEQN") %>% 
  full_join(diabetes,  by = "SEQN") %>%
  full_join(a1c,  by = "SEQN")


## RENAME VARIABLES
colnames(full_2017_2018) <- c("SEQN","SDMVPSU", "SDMVSTRA" , "survey_nr",  "gender", "age", "ethnicity", 
                               "education", "pregnancy", "born_USA",
                              "waist", "BMI", "height",
                              "SBP1", "SBP2", "SBP3", "SBP4", "DBP1",  "DBP2",  "DBP3",  "DBP4",
                              "TG", "LDL", "survey_weight",  "TC", "HDL",
                              "ever_hypertension", "ever_BP_meds", "ever_lipid_meds",
                              "now_BP_meds", "glucose",
                              "famhist_T2D",
                               "ever_heartfailure",
                              "ever_chd", "ever_angina", "ever_heartattack",
                              "ever_stroke",
                              "current_smoker", "fasting_hr",
                              "ever_diabetes", "insulin", "oral_diab_med", "hba1c")

rm(demo, body, bloodp, lipid_1, lipid_2, meds,  standard_lab,
   fam_hist, smoking, fasting, diabetes, a1c, loaded_file, 
   demo_file, body_file, bloodp_file, lipid_1_file, 
   lipid_2_file, meds_file ,standard_lab_file, fam_hist_file,
   smoking_file, fasting_file, diabetes_file, a1c_file,
   lipid_3, lipid_3_file)


full_df <- rbind(full_1999_2000,
      full_2001_2002,
      full_2003_2004,
      full_2005_2006,
      full_2007_2008,
      full_2009_2010,
      full_2011_2012,
      full_2013_2014,
      full_2015_2016,
      full_2017_2018)

saveRDS(full_df, "full_df.rds")


