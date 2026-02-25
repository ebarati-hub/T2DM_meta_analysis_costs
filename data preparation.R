#####~~~~~Data Preparation~~~~####
##### SD/SE conversion #####
# If SE is NA and CI is available, convert CI to SE using the formula: SE = (CI_Upper - CI_Lower) / (2 * 1.96)
df$se <- ifelse(is.na(df$se) & !is.na(df$ci_upper), (df$ci_upper - df$ci_lower) / (2 * 1.96), df$se)
# If SD is NA and SE is available, convert SE to SD using the formula: SD = SE*Sqrt(N)
df$sd <- ifelse(is.na(df$sd) & !is.na(df$se), df$se*sqrt(df$n), df$sd)
# If SE is NA and SD is available, convert SD to SE:
df$se <- ifelse(is.na(df$se) & !is.na(df$sd), df$sd/sqrt(df$n), df$se)

dim(df)
sum(is.na(df$sd))
#Conversion, Adjustment:
df$mean_converted = df$mean * df$IIF_GDPD* df$ICF_CAD*df$Exchange_Rate
df$sd_converted = df$sd * df$IIF_GDPD* df$ICF_CAD*df$Exchange_Rate
df$se_converted = df$se* df$IIF_GDPD* df$ICF_CAD*df$Exchange_Rate
