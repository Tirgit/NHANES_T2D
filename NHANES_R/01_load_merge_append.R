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
               "RIDEXPRG")
demo <- loaded_file[,keep_vars]

## BODY MEASUREMENTS
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BMX.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BMXWAIST", "BMXBMI")
body <- loaded_file[,keep_vars]

## BLOOD PRESSURE
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/BPX.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "BPXSAR", "BPXDAR")
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
keep_vars <- c("SEQN", "BPQ020", "BPQ040A", "BPQ090D")
meds <- loaded_file[,keep_vars]

## MEDICAL CONDITIONS, FAMILY HISTORY
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/MCQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "MCQ250A", "MCQ260AA", "MCQ260AB",
               "MCQ260AG", "MCQ260AH")
fam_hist <- loaded_file[,keep_vars]

 

## DIABETES
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/DIQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "DIQ010", "DIQ050", "DIQ070",
               "DIQ080", "DIQ090", "DIQ100", "DIQ120",
               "DIQ140", "DIQ150")
diab <- loaded_file[,keep_vars]

## CARDIOVASCULAR HEALTH
download.file("https://wwwn.cdc.gov/nchs/nhanes/1999-2000/CDQ.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "CDQ010", "CDQ020", "CDQ030", "CDQ040",
               "CDQ050", "CDQ060", "CDQ070", "CDQ080", "CDQ090")
cardio <- loaded_file[,keep_vars]







## MERGE ALL BY SEQN
full_1999_2000 <- demo %>% 
  full_join(body,  by = "SEQN") %>% 
  full_join(bloodp,  by = "SEQN") %>% 
  full_join(lipid_1,  by = "SEQN") %>% 
  full_join(lipid_2,  by = "SEQN") %>% 
  full_join(meds,  by = "SEQN") %>% 
  full_join(fam_hist,  by = "SEQN") 

## RENAME VARIABLES
colnames(full_1999_2000) <- c("SEQN", "gender", "age", "ethnicity", 
                              "income", "education", "pregnancy",
                              "waist", "BMI",
                              "SBP", "DBP",
                              "TG", "LDL", "TC", "HDL",
                              "ever_hypertension", "BP_meds", "lipid_meds",
                              "hist_T2D", "hist_T2D_mother", "hist_T2D_father",
                              "hist_T2D_brother", "hist_T2D_sister")







######################################
########## NHANES 2017-2018 ##########
######################################

## DEMOGRAPHICS
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
loaded_file <- foreign::read.xport(tf)
keep_vars <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1")
demo <- loaded_file[,keep_vars]
colnames(demo) <- c("SEQN", "gender", "age", "ethnicity")






