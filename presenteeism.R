# Packages
library(dplyr)
library(meta)
library(readxl)

# Import dataset
all <- read_excel("all.xlsx")
# filter disability cost for cases and controls:
df <- all %>%
  dplyr::filter(complication_cat == "general") %>%
  dplyr::filter(cost_type == "Productivity") %>%
  dplyr::filter(cost_subtype == "Presenteeism") %>%
  dplyr::filter(population == "T2DM" |
                  population =="noDM")

#Change the format from character to numeric
df$IIF_GDPD <- as.numeric(as.character(df$IIF_GDPD))
df$ICF_CAD <- as.numeric(as.character(df$ICF_CAD))
df$Exchange_Rate <- as.numeric(as.character(df$Exchange_Rate))
df$n <- as.numeric(as.character(df$n))

#SD calculation
# If SE is NA and CI is available, convert CI to SE using the formula: SE = (CI_Upper - CI_Lower) / (2 * 1.96)
df$se <- ifelse(is.na(df$se) & !is.na(df$ci_upper), 
                (df$ci_upper - df$ci_lower) / (2 * 1.96), 
                df$se)
# If SD is NA and SE is available, convert SE to SD using the formula: SD = SE*Sqrt(N)
df$n <- as.numeric(as.character(df$n))
df$sd <- ifelse(is.na(df$sd) & !is.na(df$se), 
                df$se * sqrt(df$n), 
                df$sd)

dim(df)
sum(is.na(df$sd))

#Conversion, Adjustment:
df$mean_converted = df$mean * df$IIF_GDPD* df$ICF_CAD* df$Exchange_Rate
df$sd_converted = df$sd * df$IIF_GDPD* df$ICF_CAD* df$Exchange_Rate
##################################################################################
####Last step is to impute sd for 621:
#lets calculate cv from study 679, and then estimate sd for cases.
# cv = sd/mean
df$cv <- df$sd_converted/df$mean_converted
cv679case <-df %>%
  filter(population == "T2DM", s_id == 679) %>%
  summarise(cv)

#impute the cv for missing ones
df[df$s_id == 621 & df$population == "T2DM", "cv"] <- cv679case

#now calculate sd using cv
df <- df %>%
  mutate(sd_converted = ifelse(is.na(sd_converted) & !is.na(cv), cv * mean_converted, sd_converted))
#keep the selected col
df_small <- df %>%
  select(s_id, author, year, population, mean_converted, sd_converted, n, complication_cat, cost_subtype, cost_type)
###################################################################################
#####################  LOG DATA  ##################################################
m_overall_logn <- metamean(n = n, mean = mean_converted, sd = sd_converted,
                           studlab = paste(author, year),
                           data = df_small,
                           sm = "MLN",           # log-normal
                           method.tau = "REML",
                           method.random.ci = "HK",           #Hartung–Knapp method, few studies & high heterogeneity
                           common = FALSE,
                           backtransf = TRUE)    # show on original scale

forest(m_overall_logn,
       sortvar = df$mean_converted, 
       prediction = FALSE, 
       refline = NA, 
       xlim = c(0, max(df$mean_converted, na.rm=TRUE)*1.15), 
       xlab = "Geometric mean cost of Presenteeism in T2DM population  (2024 CAD, back-transformed)")









