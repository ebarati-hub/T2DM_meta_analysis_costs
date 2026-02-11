#Pooling NoDM cost for the baseline
#Pool control mean cost (log scale)
library(gemtc)
library(meta)
library(data.table)
library(readxl)
library(dplyr)
df <- read_excel("absenteeism.xlsx")
df_ctrl <- subset(df, population == "noDM")
m_ctrl <- metamean(n = n, mean = mean_converted, sd = sd_converted,
                           studlab = paste(author, year),
                           data = df_ctrl,
                           sm = "MLN",           # log-normal
                           method.tau = "REML",
                           method.random.ci = "HK",           #Hartung–Knapp method, few studies & high heterogeneity
                           common = FALSE,
                           backtransf = TRUE)    # show on original scale

forest(m_ctrl,
       prediction = FALSE, 
       refline = NA,
       xlab = "Geometric mean cost of Absenteeism in noDM population  (2024 CAD, back-transformed)")




###############################################
# install.packages("writexl")  # if not installed
library(writexl)

noDM_cost <- 2625.05

df <- data.frame(
  Complication = c("amputation","blindness","chf","general","ihd","mi","rf","stroke","ulcer"),
  Ratio = c(1.65,1.90,1.77,1.20,1.21,1.31,1.37,2.17,1.58),
  Lower = c(0.68,0.66,0.59,0.70,0.40,0.53,0.47,0.86,0.54),
  Upper = c(4.01,5.60,5.27,2.12,3.65,3.21,4.20,5.54,4.87)
)

# Absolute costs
df$AbsMean  <- round(noDM_cost * df$Ratio, 2)
df$AbsLower <- round(noDM_cost * df$Lower, 2)
df$AbsUpper <- round(noDM_cost * df$Upper, 2)

# Add formatted column
df$Result <- sprintf("%.0f (%.0f–%.0f)", df$AbsMean, df$AbsLower, df$AbsUpper)

# Export
write_xlsx(df, "Absolute_Costs_NMA.xlsx")

df
