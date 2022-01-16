# Set working directory
setwd("~/GitHub/NHANES_T2D/Data")

# load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(tidyverse)

# load results from NHANES
result_df <- readRDS("RESULTS_df.rds")
result_df$baseline_year <- NULL

# load incidence rates / 1000
diab_incidence <- read_xlsx("~/GitHub/NHANES_T2D/Data/diab_incidence.xlsx")

# Calculate SEs from the diab_incidence data frame
# SE = (UL - mean) / 1.96

diab_incidence <- diab_incidence |> 
  mutate(SE_all = (All_uCI - All) / 1.96,
         SE_Hispanic = (Hispanic_uCI - Hispanic) / 1.96,
         SE_White = (White_uCI - White) / 1.96,
         SE_Black = (Black_uCI - Black) / 1.96,
         Variance_all = SE_all^2,
         Variance_Hispanic = SE_Hispanic^2,
         Variance_White = SE_White^2,
         Variance_Black = SE_Black^2)


# FRAMINGHAM, SAN ANTONIO: EIGHT YEAR INCIDENCES
# 2007 - 2018 cumulative incidences:

all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

all_inc_var <- c()
hispanic_inc_var <- c()
white_inc_var <- c()
black_inc_var <- c()

# For the cumulative incidence 8-years
for (i in 0:11) {
  all_inc <- c(all_inc, sum(diab_incidence[(1+i):(8+i),2]))
  hispanic_inc <- c(hispanic_inc, sum(diab_incidence[(1+i):(8+i),4]))
  white_inc <- c(white_inc, sum(diab_incidence[(1+i):(8+i),6]))
  black_inc <- c(black_inc, sum(diab_incidence[(1+i):(8+i),8]))
}

# For the standard errors 8-years (calculate the 1.96 * SE)
for (i in 0:11) {
  all_inc_var <- c(all_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(8+i),14])))
  hispanic_inc_var <- c(hispanic_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(8+i),15])))
  white_inc_var <- c(white_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(8+i),16])))
  black_inc_var <- c(black_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(8+i),17])))
}


# Assign a new data frame with both the cumulative incidence and the bounds
all_inc_new <- as.data.frame(cbind(all_inc, all_inc_var))
hispanic_inc_new <- as.data.frame(cbind(hispanic_inc, hispanic_inc_var))
white_inc_new <- as.data.frame(cbind(white_inc, white_inc_var))
black_inc_new <- as.data.frame(cbind(black_inc, black_inc_var))

# Rename the columns and calculate the confidence intervals now

# For everyone
all_inc_new <- all_inc_new |> 
  rename(Cumulative_Incidence = 'all_inc', Bound = 'all_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'All') 
all_inc_new$Ethnicity <- as.factor(all_inc_new$Ethnicity)

# For Hispanics
hispanic_inc_new <- hispanic_inc_new |> 
  rename(Cumulative_Incidence = 'hispanic_inc', Bound = 'hispanic_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Hispanic')
hispanic_inc_new$Ethnicity <- as.factor(hispanic_inc_new$Ethnicity)

# For Whites
white_inc_new <- white_inc_new |> 
  rename(Cumulative_Incidence = 'white_inc', Bound = 'white_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)

# We can merge the dataframes now
incidence_8year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_8year <- incidence_8year |> rename(Incidence_8year = 'Cumulative_Incidence')


# DESIR, ARIC: NINE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:


all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

all_inc_var <- c()
hispanic_inc_var <- c()
white_inc_var <- c()
black_inc_var <- c()

# For the cumulative incidence 9-years
for (i in 0:10) {
  all_inc <- c(all_inc, sum(diab_incidence[(1+i):(9+i),2]))
  hispanic_inc <- c(hispanic_inc, sum(diab_incidence[(1+i):(9+i),4]))
  white_inc <- c(white_inc, sum(diab_incidence[(1+i):(9+i),6]))
  black_inc <- c(black_inc, sum(diab_incidence[(1+i):(9+i),8]))
}


# For the standard errors 9-years (calculate the 1.96 * SE)
for (i in 0:10) {
  all_inc_var <- c(all_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(9+i),14])))
  hispanic_inc_var <- c(hispanic_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(9+i),15])))
  white_inc_var <- c(white_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(9+i),16])))
  black_inc_var <- c(black_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(9+i),17])))
}


# Assign a new data frame with both the cumulative incidence and the bounds
all_inc_new <- as.data.frame(cbind(all_inc, all_inc_var))
hispanic_inc_new <- as.data.frame(cbind(hispanic_inc, hispanic_inc_var))
white_inc_new <- as.data.frame(cbind(white_inc, white_inc_var))
black_inc_new <- as.data.frame(cbind(black_inc, black_inc_var))


# Rename the columns and calculate the confidence intervals now

# For everyone
all_inc_new <- all_inc_new |> 
  rename(Cumulative_Incidence = 'all_inc', Bound = 'all_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'All') 
all_inc_new$Ethnicity <- as.factor(all_inc_new$Ethnicity)

# For Hispanics
hispanic_inc_new <- hispanic_inc_new |> 
  rename(Cumulative_Incidence = 'hispanic_inc', Bound = 'hispanic_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Hispanic')
hispanic_inc_new$Ethnicity <- as.factor(hispanic_inc_new$Ethnicity)

# For Whites
white_inc_new <- white_inc_new |> 
  rename(Cumulative_Incidence = 'white_inc', Bound = 'white_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)


# We can merge the dataframes now
incidence_9year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_9year <- incidence_9year |> rename(Incidence_9year = 'Cumulative_Incidence')


# EGATS: TWELVE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:

all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

all_inc_var <- c()
hispanic_inc_var <- c()
white_inc_var <- c()
black_inc_var <- c()

# For the cumulative incidence 12-years
for (i in 0:7) {
  all_inc <- c(all_inc, sum(diab_incidence[(1+i):(12+i),2]))
  hispanic_inc <- c(hispanic_inc, sum(diab_incidence[(1+i):(12+i),4]))
  white_inc <- c(white_inc, sum(diab_incidence[(1+i):(12+i),6]))
  black_inc <- c(black_inc, sum(diab_incidence[(1+i):(12+i),8]))
}


# For the standard errors 12-years (calculate the 1.96 * SE)
for (i in 0:7) {
  all_inc_var <- c(all_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(12+i),14])))
  hispanic_inc_var <- c(hispanic_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(12+i),15])))
  white_inc_var <- c(white_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(12+i),16])))
  black_inc_var <- c(black_inc_var, 1.96 * sqrt(sum(diab_incidence[(1+i):(12+i),17])))
}


# Assign a new data frame with both the cumulative incidence and the bounds
all_inc_new <- as.data.frame(cbind(all_inc, all_inc_var))
hispanic_inc_new <- as.data.frame(cbind(hispanic_inc, hispanic_inc_var))
white_inc_new <- as.data.frame(cbind(white_inc, white_inc_var))
black_inc_new <- as.data.frame(cbind(black_inc, black_inc_var))

# Rename the columns and calculate the confidence intervals now

# For everyone
all_inc_new <- all_inc_new |> 
  rename(Cumulative_Incidence = 'all_inc', Bound = 'all_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'All') 
all_inc_new$Ethnicity <- as.factor(all_inc_new$Ethnicity)

# For Hispanics
hispanic_inc_new <- hispanic_inc_new |> 
  rename(Cumulative_Incidence = 'hispanic_inc', Bound = 'hispanic_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Hispanic')
hispanic_inc_new$Ethnicity <- as.factor(hispanic_inc_new$Ethnicity)

# For Whites
white_inc_new <- white_inc_new |> 
  rename(Cumulative_Incidence = 'white_inc', Bound = 'white_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)


# We can merge the dataframes now
incidence_12year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_12year <- incidence_12year |> rename(Incidence_12year = 'Cumulative_Incidence')



# START FROM HERE:


# add column: name of "model", e.g 8-year cumulative incidence
# add column: year the number is applicable for.
incidence_8year
incidence_9year
incidence_12year

# merge with result_df
result_df


# merge observed with calculated incidences
df <- rbind(result_df, eight_yr_inc, nine_yr_inc, twelve_yr_inc)

# visualization Framingham
df_model <- df[df$model == "Framingham" | df$model == "8-yr-incidence",]

p <- ggplot(df_model, aes(x=year, y=avg_pred, col=model)) +
  geom_point() +
  facet_grid(~ethnicity) +
  theme_minimal()

png("Framingham_pred.png", width = 600, height = 300)
p
dev.off()

valid_years <- c(2007,2009,2011,2013,2015,2017)
df_y <- df_model[df_model$year %in% valid_years,]
df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "ethnicity"), timevar = c("model"))
df_y_wide$diff <- df_y_wide$avg_pred.Framingham - df_y_wide$`avg_pred.8-yr-incidence`

p <- ggplot(df_y_wide, aes(x=year, y=diff, col=ethnicity)) +
  geom_point() +
  geom_hline(yintercept=c(0), linetype='dashed') +
  ylab("Delta predicted score") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal()

png("Framingham_diff.png", width = 600, height = 300)
p
dev.off()



# visualization San Antonio
df_model <- df[df$model == "San Antonio" | df$model == "8-yr-incidence",]

p <- ggplot(df_model, aes(x=year, y=avg_pred, col=model)) +
  geom_point() +
  facet_grid(~ethnicity) +
  theme_minimal()

png("SanAntonio_pred.png", width = 600, height = 300)
p
dev.off()

valid_years <- c(2007,2009,2011,2013,2015,2017)
df_y <- df_model[df_model$year %in% valid_years,]
df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "ethnicity"), timevar = c("model"))
df_y_wide$diff <- df_y_wide$`avg_pred.San Antonio` - df_y_wide$`avg_pred.8-yr-incidence`

p <- ggplot(df_y_wide, aes(x=year, y=diff, col=ethnicity)) +
  geom_point() +
  geom_hline(yintercept=c(0), linetype='dashed') +
  ylab("Delta predicted score") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal()

png("SanAntonio_diff.png", width = 600, height = 300)
p
dev.off()

# visualization ARIC
df_model <- df[df$model == "ARIC" | df$model == "9-yr-incidence",]

p <- ggplot(df_model, aes(x=year, y=avg_pred, col=model)) +
  geom_point() +
  facet_grid(~ethnicity) +
  theme_minimal()

png("ARIC_pred.png", width = 600, height = 300)
p
dev.off()

valid_years <- c(2008,2010,2012,2014,2016,2018)
df_y <- df_model[df_model$year %in% valid_years,]
df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "ethnicity"), timevar = c("model"))
df_y_wide$diff <- df_y_wide$avg_pred.ARIC - df_y_wide$`avg_pred.9-yr-incidence`

p <- ggplot(df_y_wide, aes(x=year, y=diff, col=ethnicity)) +
  geom_point() +
  geom_hline(yintercept=c(0), linetype='dashed') +
  ylab("Delta predicted score") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal()

png("ARIC_diff.png", width = 600, height = 300)
p
dev.off()

# visualization DESIR
df_model <- df[df$model == "DESIR" | df$model == "9-yr-incidence",]

p <- ggplot(df_model, aes(x=year, y=avg_pred, col=model)) +
  geom_point() +
  facet_grid(~ethnicity) +
  theme_minimal()

png("DESIR_pred.png", width = 600, height = 300)
p
dev.off()

valid_years <- c(2008,2010,2012,2014,2016,2018)
df_y <- df_model[df_model$year %in% valid_years,]
df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "ethnicity"), timevar = c("model"))
df_y_wide$diff <- df_y_wide$avg_pred.DESIR - df_y_wide$`avg_pred.9-yr-incidence`

p <- ggplot(df_y_wide, aes(x=year, y=diff, col=ethnicity)) +
  geom_point() +
  geom_hline(yintercept=c(0), linetype='dashed') +
  ylab("Delta predicted score") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal()

png("DESIR_diff.png", width = 600, height = 300)
p
dev.off()

# visualization EGATS
df_model <- df[df$model == "EGATS" | df$model == "12-yr-incidence",]

p <- ggplot(df_model, aes(x=year, y=avg_pred, col=model)) +
  geom_point() +
  facet_grid(~ethnicity) +
  theme_minimal()

png("EGATS_pred.png", width = 600, height = 300)
p
dev.off()

valid_years <- c(2011,2013,2015,2017)
df_y <- df_model[df_model$year %in% valid_years,]
df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "ethnicity"), timevar = c("model"))
df_y_wide$diff <- df_y_wide$avg_pred.EGATS - df_y_wide$`avg_pred.12-yr-incidence`

p <- ggplot(df_y_wide, aes(x=year, y=diff, col=ethnicity)) +
  geom_point() +
  geom_hline(yintercept=c(0), linetype='dashed') +
  ylab("Delta predicted score") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal()

png("EGATS_diff.png", width = 600, height = 300)
p
dev.off()
