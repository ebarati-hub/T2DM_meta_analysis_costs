# Packages
library(dplyr)
library(metafor)
library(readr)
library(meta)
library(readxl)

# Import dataset
all <- read_excel("all.xlsx")

# filter disability cost for cases and controls:
df <- all %>%
  dplyr::filter(complication_cat == "general") %>%
  dplyr::filter(cost_type == "Benefits") %>%
  dplyr::filter(cost_subtype == "Sickness benefits and work accidents/occupational diseases" |
                  cost_subtype == "Sick benefits") %>%
  dplyr::filter(population == "T2DM" |
                  population =="noDM")

unique(df$s_id)


#Change the format from character to numeric
df$IIF_GDPD <- as.numeric(as.character(df$IIF_GDPD))
df$ICF_CAD <- as.numeric(as.character(df$ICF_CAD))
df$Exchange_Rate <- as.numeric(as.character(df$Exchange_Rate))
df$n <- as.numeric(as.character(df$n))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# study 599 reports cost  from 2006 to 2015. 
# study 781 reports cost  from 2009 to 2010.
# We must first do inflation-adjustment and 
# convert to the common currency, then pool to derive the average annual cost.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#pool mean for study 599. Action: sum the adjusted means and divide by 10
# sd is NR
# N is the same

case599 <- df %>%
  filter(population == "T2DM", s_id == 599) %>%
  summarise(annual_mean = sum(mean_converted, na.rm = TRUE) / 10) %>%
  pull(annual_mean)


control599 <- df %>%
  filter(population == "noDM", s_id == 599) %>%
  summarise(annual_mean = sum(mean_converted, na.rm = TRUE) / 10) %>%
  pull(annual_mean)


df <- df %>% 
  add_row(
    s_id = 599,
    year = 2019,
    population = "T2DM",
    mean_converted = case599,
    n = 170013,
    cost_type = "Benefits",
    cost_subtype = "Sickness benefits and work accidents/occupational diseases",
    complication_cat = "general",
    author = "Baudot",
    IC = "ready"
  )


df <- df %>% 
  add_row(
    s_id = 599,
    year = 2019,
    population = "noDM",
    mean_converted = control599,
    n = 510022,
    cost_type = "Benefits",
    cost_subtype = "Sickness benefits and work accidents/occupational diseases",
    complication_cat = "general",
    author = "Baudot",
    IC = "ready"
  )


##################################################################################
#pool mean for study 781. Action: sum the adjusted means and divide by 2
# sd is NR
# N is the same

case781 <- df %>%
  filter(population == "T2DM", s_id == 781) %>%
  summarise(annual_mean = sum(mean_converted, na.rm = TRUE) / 2) %>%
  pull(annual_mean)


control781 <- df %>%
  filter(population == "noDM", s_id == 781) %>%
  summarise(annual_mean = sum(mean_converted, na.rm = TRUE) / 2) %>%
  pull(annual_mean)


df <- df %>% 
  add_row(
    s_id = 781,
    year = 2017,
    population = "T2DM",
    mean_converted = case781,
    n = 301000,
    cost_type = "Benefits",
    cost_subtype = "Sick benefits",
    complication_cat = "general",
    author = "Jacobs",
    IC = "ready"
  )


df <- df %>% 
  add_row(
    s_id = 781,
    year = 2017,
    population = "noDM",
    mean_converted = control781,
    n = 4300000,
    cost_type = "Benefits",
    cost_subtype = "Sick benefits",
    complication_cat = "general",
    author = "Jacobs",
    IC = "ready"
  )




# lets remove extra rows
df <- subset(df, IC == "ready")


####Last step is to impute sd
#neither of the studies reported any kind of variability measure.
#Lets assume cv=0.5, then sd = 50% mean.
df$sd_converted = df$mean_converted * 0.5

#keep the selected col
df_small <- df %>%
  select(s_id, author, year, population, mean_converted, sd_converted, n, complication_cat, cost_subtype, cost_type)
#############################################################################
#change the format form long to wide and rename the columns
library(tidyr)

df_wide <- df %>%
  mutate(population = recode(population,
                             "T2DM" = "case",
                             "noDM" = "control")) %>% 
  select(s_id, author, year, population,
         mean_converted, sd_converted, n) %>%
  pivot_wider(
    names_from = population,
    values_from = c(mean_converted, sd_converted, n),
    names_glue = "{.value}_{population}"
  )

df_wide <- df_wide %>%
  rename(
    mean_case = mean_converted_case,
    sd_case   = sd_converted_case,
    n_case    = n_case,
    mean_control = mean_converted_control,
    sd_control   = sd_converted_control,
    n_control    = n_control
  )



#MA using metacon and in log scale
library(meta)

m_cost <- metacont(
  n.e = n_case, mean.e = mean_case, sd.e = sd_case,
  n.c = n_control, mean.c = mean_control, sd.c = sd_control,
  studlab = paste(author, year),
  data = df_wide,
  sm = "ROM",              # ratio of means = log scale internally
  method.tau = "REML",
  method.random.ci = "HK"
)

summary(m_cost)
forest(m_cost,common = F, 
       main = "Pooled Ratio of Mean Disability Benefits Costs (T2DM vs Controls)",
       xlab = "Ratio of Means",
       backtransf = TRUE)
###############################################################################
#lets use metagen this time
library(dplyr)

df_es <- df_wide %>%
  mutate(
    # log ratio of means (cases vs controls)
    yi  = log(mean_case) - log(mean_control),
    
    # variance of log-mean via delta method
    vi  = (sd_case^2    / (n_case    * mean_case^2)) +
      (sd_control^2 / (n_control * mean_control^2)),
    
    sei = sqrt(vi),
    studlab = paste(author, year)
  )
library(meta)

m_rom <- metagen(
  TE    = df_es$yi,
  seTE  = df_es$sei,
  studlab = df_es$studlab,
  sm = "ROM",            # tells meta we're on the ratio-of-means scale
  method.tau = "REML",
  method.random.ci = TRUE            # Hartung–Knapp (good with 2 studies)
)



summary(m_rom)

# Pooled ratio of means and 95% CI:
pooled_ratio   <- exp(m_rom$TE.random)
lower_ratio    <- exp(m_rom$lower.random)
upper_ratio    <- exp(m_rom$upper.random)
c(pooled_ratio, lower_ratio, upper_ratio)

forest(m_rom, backtransf = TRUE,
       xlab = "Ratio of Mean Costs (Cases vs Controls)",
       leftlabs = c("Study", "log ROM", "SE"))
#################################################################################
#################################################################################
#################################################################################
#Pool control mean cost (log scale)
m_ctrl <- metamean(
  n = n_control, mean = mean_control, sd = sd_control,
  studlab = s_id, data = df_wide,
  sm = "MLN", method.tau = "REML",
  method.random.ci = "HK", backtransf = TRUE
)
summary(m_ctrl)
forest(m_ctrl)


ctrl_hat  <- exp(m_ctrl$TE.random)       # pooled control mean ($)_ Back-transformed
ctrl_lci  <- exp(m_ctrl$lower.random)
ctrl_uci  <- exp(m_ctrl$upper.random)



#pooled ROM
# m_rom from metacont
rom_hat <- exp(m_rom$TE.random)       # Back-transformed
rom_lci <- exp(m_rom$lower.random)
rom_uci <- exp(m_rom$upper.random)

#Point estimate for case mean
case_hat <- rom_hat * ctrl_hat

#95% CI for case mean
# get log-scale SEs from the CIs
se_log_rom  <- (log(rom_uci)  - log(rom_lci)) / (2 * 1.96)
se_log_ctrl <- (log(ctrl_uci) - log(ctrl_lci)) / (2 * 1.96)

mu_log <- log(rom_hat) + log(ctrl_hat)
se_log <- sqrt(se_log_rom^2 + se_log_ctrl^2)  # ignores correlation

case_lci <- exp(mu_log - 1.96 * se_log)
case_uci <- exp(mu_log + 1.96 * se_log)

c(case_hat = case_hat, case_lci = case_lci, case_uci = case_uci)
################################################################################
#Summary Table
summary_table <- data.frame(
  Group = c("Control", "Case", "Ratio of Means"),
  Estimate = c(ctrl_hat, case_hat, rom_hat),
  Lower_CI = c(ctrl_lci, case_lci, rom_lci),
  Upper_CI = c(ctrl_uci, case_uci, rom_uci)
)

print(summary_table)
library(writexl)
write_xlsx(summary_table, "pooled_cost_results.xlsx")









