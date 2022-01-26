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
result_df$estimate <- result_df$pooled.avg
result_df$estimate_lCI <- result_df$estimate - 1.96*result_df$pooled.se
result_df$estimate_uCI <- result_df$estimate + 1.96*result_df$pooled.se
result_df <- result_df |>
  select(-pooled.avg, -pooled.se) |>
  select(estimate, estimate_lCI, estimate_uCI,
         Ethnicity, Model, year)
result_df$Ethnicity[result_df$Ethnicity == "Black"] <- "non-Hispanic Black"
result_df$Ethnicity[result_df$Ethnicity == "White"] <- "non-Hispanic White"

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
  mutate(Ethnicity = 'non-Hispanic White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'non-Hispanic Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)

# We can merge the dataframes now
incidence_8year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_8year <- incidence_8year |> rename(Incidence_8year = 'Cumulative_Incidence')

model_vals <- rep("8-yr-incidence",nrow(incidence_8year))
year_vals <- rep(2007:2018,4)
incidence_8year <- as.data.frame(cbind(incidence_8year,
                                    model = model_vals,
                                    year = year_vals))
colnames(incidence_8year) <- c("estimate","estimate_lCI",
                               "estimate_uCI","Ethnicity",
                               "Model", "year")

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
  mutate(Ethnicity = 'non-Hispanic White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'non-Hispanic Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)


# We can merge the dataframes now
incidence_9year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_9year <- incidence_9year |> rename(Incidence_9year = 'Cumulative_Incidence')

model_vals <- rep("9-yr-incidence",nrow(incidence_9year))
year_vals <- rep(2008:2018,4)
incidence_9year <- as.data.frame(cbind(incidence_9year,
                                       model = model_vals,
                                       year = year_vals))
colnames(incidence_9year) <- c("estimate","estimate_lCI",
                               "estimate_uCI","Ethnicity",
                               "Model", "year")


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
  mutate(Ethnicity = 'non-Hispanic White')
white_inc_new$Ethnicity <- as.factor(white_inc_new$Ethnicity)

# For Blacks
black_inc_new <- black_inc_new |> 
  rename(Cumulative_Incidence = 'black_inc', Bound = 'black_inc_var') |> 
  mutate(LI = Cumulative_Incidence - Bound, UI = Cumulative_Incidence + Bound) |> 
  select(-Bound) |> 
  mutate(Ethnicity = 'non-Hispanic Black')
black_inc_new$Ethnicity <- as.factor(black_inc_new$Ethnicity)


# We can merge the dataframes now
incidence_12year <- bind_rows(all_inc_new, hispanic_inc_new, white_inc_new, black_inc_new)
incidence_12year <- incidence_12year |> rename(Incidence_12year = 'Cumulative_Incidence')

model_vals <- rep("12-yr-incidence",nrow(incidence_12year))
year_vals <- rep(2011:2018,4)
incidence_12year <- as.data.frame(cbind(incidence_12year,
                                       model = model_vals,
                                       year = year_vals))
colnames(incidence_12year) <- c("estimate","estimate_lCI",
                               "estimate_uCI","Ethnicity",
                               "Model", "year")


# merge observed with calculated incidences
df <- rbind(result_df, incidence_8year, incidence_9year, incidence_12year)




########################################
########################################
############### PLOTTING ###############
########################################
########################################


# directory to Plots
setwd("~/GitHub/NHANES_T2D/Plots")

# visualization Framingham
df_model <- df[df$Model == "Framingham" | df$Model == "8-yr-incidence",]
valid_years <- c(2007,2009,2011,2013,2015,2017)
df_y <- df_model[df_model$year %in% valid_years,]

# Make this a factor to change the name of the levels
df_y$Ethnicity <- as.factor(df_y$Ethnicity)

# Change the level names (plurals)

levels(df_y$Ethnicity)[2] <- 'Hispanics'
levels(df_y$Ethnicity)[3] <- 'Non-Hispanic Blacks'
levels(df_y$Ethnicity)[4] <- 'Non-Hispanic Whites'


p <- ggplot(df_y, aes(x=year, y=estimate, col=Model)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin=estimate_lCI,ymax=estimate_uCI), size = 2, width = 0.5) +
  facet_grid(~Ethnicity, labeller = label_wrap_gen(width=10)) +
  theme_minimal() +    
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        strip.text = element_text(size=rel(3)),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  scale_x_continuous(breaks=valid_years) +
  xlab("") + 
  ylab("Average Predicted Incidence") +
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("Framingham_pred.png", width = 1200, height = 600)
p
dev.off()

df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "Ethnicity"), timevar = c("Model"))
df_y_wide$ratio <- df_y_wide$estimate.Framingham / df_y_wide$`estimate.8-yr-incidence`

q <- ggplot(df_y_wide, aes(x=year, y=ratio, col=Ethnicity)) +
  geom_point(size = 7) +
  geom_hline(yintercept=c(1), linetype='dashed') +
  ylab("API / CI") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal() +
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  xlab("") + 
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("Framingham_ratio.png", width = 1200, height = 300)
q
dev.off()

writexl::write_xlsx(df_y_wide, "~/GitHub/NHANES_T2D/Manuscript_items/not_needed/Framingham_table.xlsx")


# visualization San Antonio
df_model <- df[df$Model == "Antonio" | df$Model == "8-yr-incidence",]
valid_years <- c(2007,2009,2011,2013,2015,2017)
df_y <- df_model[df_model$year %in% valid_years,]


# Make this a factor to change the name of the levels
df_y$Ethnicity <- as.factor(df_y$Ethnicity)

# Change the level names (plurals)

levels(df_y$Ethnicity)[2] <- 'Hispanics'
levels(df_y$Ethnicity)[3] <- 'Non-Hispanic Blacks'
levels(df_y$Ethnicity)[4] <- 'Non-Hispanic Whites'

# Make Model a factor as well

df_y$Model <- as.factor(df_y$Model)

levels(df_y$Model)[2] <- 'San Antonio'

p <- ggplot(df_y, aes(x=year, y=estimate, col=Model)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin=estimate_lCI,ymax=estimate_uCI), size = 2, width = 0.5) +
  facet_grid(~Ethnicity, labeller = label_wrap_gen(width=10)) +
  theme_minimal() +    
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        strip.text = element_text(size=rel(3)),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  scale_x_continuous(breaks=valid_years) +
  xlab("") + 
  ylab("Average Predicted Incidence") +
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("SanAntonio_pred.png", width = 1200, height = 600)
p
dev.off()

df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "Ethnicity"), timevar = c("Model"))
df_y_wide$ratio <- df_y_wide$`estimate.San Antonio` / df_y_wide$`estimate.8-yr-incidence`

q <- ggplot(df_y_wide, aes(x=year, y=ratio, col=Ethnicity)) +
  geom_point(size = 7) +
  geom_hline(yintercept=c(1), linetype='dashed') +
  ylab("API / CI") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal() +
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  xlab("") + 
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("SanAntonio_ratio.png", width = 1200, height = 300)
q
dev.off()


writexl::write_xlsx(df_y_wide, "~/GitHub/NHANES_T2D/Manuscript_items/not_needed/Antonio_table.xlsx")


# visualization ARIC
df_model <- df[df$Model == "ARIC" | df$Model == "9-yr-incidence",]
valid_years <- c(2008,2010,2012,2014,2016,2018)
df_y <- df_model[df_model$year %in% valid_years,]


# Make this a factor to change the name of the levels
df_y$Ethnicity <- as.factor(df_y$Ethnicity)

# Change the level names (plurals)

levels(df_y$Ethnicity)[2] <- 'Hispanics'
levels(df_y$Ethnicity)[3] <- 'Non-Hispanic Blacks'
levels(df_y$Ethnicity)[4] <- 'Non-Hispanic Whites'


p <- ggplot(df_y, aes(x=year, y=estimate, col=Model)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin=estimate_lCI,ymax=estimate_uCI), size = 2, width = 0.5) +
  facet_grid(~Ethnicity, labeller = label_wrap_gen(width=10)) +
  theme_minimal() +    
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        strip.text = element_text(size=rel(3)),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  scale_x_continuous(breaks=valid_years) +
  xlab("") + 
  ylab("Average Predicted Incidence") +
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))

png("ARIC_pred.png", width = 1200, height = 600)
p
dev.off()

df_y_wide <- reshape(df_y, direction = "wide",
                     idvar = c("year", "Ethnicity"), timevar = c("Model"))
df_y_wide$ratio <- df_y_wide$estimate.ARIC / df_y_wide$`estimate.9-yr-incidence`

q <- ggplot(df_y_wide, aes(x=year, y=ratio, col=Ethnicity)) +
  geom_point(size = 7) +
  geom_hline(yintercept=c(1), linetype='dashed') +
  ylab("API / CI") +
  scale_x_continuous(breaks = valid_years) +
  theme_minimal() +
  theme(axis.text.y=element_text(size=rel(3)),
        axis.text.x=element_text(size=rel(3), angle=45),
        axis.title.x = element_text(size=rel(3)),
        axis.title.y = element_text(size=rel(3)),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3))) + 
  xlab("") + 
  scale_color_manual(values=c("#1B9E77","#D95F02","#7570B3","#E7298A"))


png("ARIC_ratio.png", width = 1200, height = 300)
q
dev.off()

writexl::write_xlsx(df_y_wide, "~/GitHub/NHANES_T2D/Manuscript_items/not_needed/ARIC_table.xlsx")







# # visualization DESIR
# df_model <- df[df$Model == "DESIR" | df$Model == "9-yr-incidence",]
# 
# p <- ggplot(df_model, aes(x=year, y=estimate, col=Model)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate_lCI,ymax=estimate_uCI)) +
#   facet_grid(~Ethnicity) +
#   theme_minimal()
# 
# png("DESIR_pred.png", width = 1200, height = 600)
# p
# dev.off()
# 
# valid_years <- c(2008,2010,2012,2014,2016,2018)
# df_y <- df_model[df_model$year %in% valid_years,]
# df_y_wide <- reshape(df_y, direction = "wide",
#                      idvar = c("year", "Ethnicity"), timevar = c("Model"))
# df_y_wide$diff <- df_y_wide$estimate.DESIR - df_y_wide$`estimate.9-yr-incidence`
# df_y_wide$ratio <- df_y_wide$estimate.DESIR / df_y_wide$`estimate.9-yr-incidence`
# 
# p <- ggplot(df_y_wide, aes(x=year, y=diff, col=Ethnicity)) +
#   geom_point() +
#   geom_hline(yintercept=c(0), linetype='dashed') +
#   ylab("Delta predicted score") +
#   scale_x_continuous(breaks = valid_years) +
#   theme_minimal()
# 
# q <- ggplot(df_y_wide, aes(x=year, y=ratio, col=Ethnicity)) +
#   geom_point() +
#   geom_hline(yintercept=c(1), linetype='dashed') +
#   ylab("Ratio predicted score") +
#   scale_x_continuous(breaks = valid_years) +
#   theme_minimal()
# 
# png("DESIR_diff.png", width = 1200, height = 600)
# p
# dev.off()
# png("DESIR_ratio.png", width = 1200, height = 600)
# q
# dev.off()
# 
# # visualization EGATS
# df_model <- df[df$Model == "EGATS" | df$Model == "12-yr-incidence",]
# 
# p <- ggplot(df_model, aes(x=year, y=estimate, col=Model)) +
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate_lCI,ymax=estimate_uCI)) +
#   facet_grid(~Ethnicity) +
#   theme_minimal()
# 
# png("EGATS_pred.png", width = 1200, height = 600)
# p
# dev.off()
# 
# valid_years <- c(2011,2013,2015,2017)
# df_y <- df_model[df_model$year %in% valid_years,]
# df_y_wide <- reshape(df_y, direction = "wide",
#                      idvar = c("year", "Ethnicity"), timevar = c("Model"))
# df_y_wide$diff <- df_y_wide$estimate.EGATS - df_y_wide$`estimate.12-yr-incidence`
# df_y_wide$ratio <- df_y_wide$estimate.EGATS / df_y_wide$`estimate.12-yr-incidence`
# 
# p <- ggplot(df_y_wide, aes(x=year, y=diff, col=Ethnicity)) +
#   geom_point() +
#   geom_hline(yintercept=c(0), linetype='dashed') +
#   ylab("Delta predicted score") +
#   scale_x_continuous(breaks = valid_years) +
#   theme_minimal()
# 
# q <- ggplot(df_y_wide, aes(x=year, y=ratio, col=Ethnicity)) +
#   geom_point() +
#   geom_hline(yintercept=c(1), linetype='dashed') +
#   ylab("Ratio predicted score") +
#   scale_x_continuous(breaks = valid_years) +
#   theme_minimal()
# 
# png("EGATS_diff.png", width = 1200, height = 600)
# p
# dev.off()
# png("EGATS_ratio.png", width = 1200, height = 600)
# q
# dev.off()
