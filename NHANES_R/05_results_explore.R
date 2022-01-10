# Set working directory
setwd("~/GitHub/NHANES_T2D")

# load necessary libraries
library(readxl)

# load incidence rates / 1000
diab_incidence <- read_xlsx("diab_incidence.xlsx")
diab_survival <- cbind(diab_incidence[,1], 1-diab_incidence[,2:5])
1000

1 - prod(diab_survival[1:9,2])
