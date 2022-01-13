# Set working directory
setwd("~/GitHub/NHANES_T2D")

# load necessary libraries
library(readxl)
library(ggplot2)
library(reshape2)

# load results from NHANES
result_df <- readRDS("NHANES_R/result_df.rds")
result_df$baseline_year <- NULL

# load incidence rates / 1000
diab_incidence <- read_xlsx("Data/diab_incidence.xlsx")

# FRAMINGHAM, SAN ANTONIO: EIGHT YEAR INCIDENCES
# 2007 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:11) {
all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(8+i),2]))
hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(1+i):(8+i),3]))
white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(8+i),4]))
black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(8+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("8-yr-incidence",length_inc)
year_vals <- rep(2007:2018,4)
ethnicity_vals <- c(rep("All",12),rep("Hispanic",12),rep("White",12),rep("Black",12))
eight_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                 model = model_vals,
                                 ethnicity = ethnicity_vals,
                                 year = year_vals))
eight_yr_inc$avg_pred <- as.numeric(eight_yr_inc$avg_pred)
eight_yr_inc$year <- as.numeric(eight_yr_inc$year)


# DESIR, ARIC: NINE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:10) {
  all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(9+i),2]))
  hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(1+i):(9+i),3]))
  white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(9+i),4]))
  black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(9+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("9-yr-incidence",length_inc)
year_vals <- rep(2008:2018,4)
ethnicity_vals <- c(rep("All",11),rep("Hispanic",11),rep("White",11),rep("Black",11))
nine_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                    model = model_vals,
                                    ethnicity = ethnicity_vals,
                                    year = year_vals))
nine_yr_inc$avg_pred <- as.numeric(nine_yr_inc$avg_pred)
nine_yr_inc$year <- as.numeric(nine_yr_inc$year)



# EGATS: TWELVE YEAR INCIDENCES
# 2008 - 2018 cumulative incidences:
all_inc <- c()
hispanic_inc <- c()
white_inc <- c()
black_inc <- c()

for (i in 0:7) {
  all_inc <- c(all_inc, 1 - prod(diab_survival[(1+i):(12+i),2]))
  hispanic_inc <- c(hispanic_inc, 1 - prod(diab_survival[(12+i):(6+i),3]))
  white_inc <- c(white_inc, 1 - prod(diab_survival[(1+i):(12+i),4]))
  black_inc <- c(black_inc, 1 - prod(diab_survival[(1+i):(12+i),5]))
}

cbind()

length_inc <- length(all_inc)*4
# generate result dataframe
model_vals <- rep("12-yr-incidence",length_inc)
year_vals <- rep(2011:2018,4)
ethnicity_vals <- c(rep("All",8),rep("Hispanic",8),rep("White",8),rep("Black",8))
twelve_yr_inc <- as.data.frame(cbind(avg_pred = c(all_inc,hispanic_inc,white_inc,black_inc),
                                   model = model_vals,
                                   ethnicity = ethnicity_vals,
                                   year = year_vals))
twelve_yr_inc$avg_pred <- as.numeric(twelve_yr_inc$avg_pred)
twelve_yr_inc$year <- as.numeric(twelve_yr_inc$year)


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
