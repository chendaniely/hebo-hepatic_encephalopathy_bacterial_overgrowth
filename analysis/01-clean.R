library(readxl)
library(here)

source(here::here('R/recode.R'))
source(here::here('R/rise.R'))

df <- read_excel(here::here('data/original/HEBOnewworksheet5.21.xlsx'))

# replace 999 with NA missing value -----
df[df == 999] <- NA

# factorize -----
df$Etiology <- as.factor(df$Etiology)
df$Lactulose <- as.factor(df$Lactulose)
df$Gender <- as.factor(df$Gender)
df$Depression <- as.factor(df$Depression)
df$SBP <- as.factor(df$SBP)
df$DM <- as.factor(df$DM)
df$ETOH <- as.factor(df$ETOH)
df$Cclass <- as.factor(df$Cclass)
df$HEStage <- as.factor(df$HEStage)
df$Ascites <- as.factor(df$Ascites)
df$EVB <- as.factor(df$EVB)
df$Varices <- as.factor(df$Varices)
df$PPIUse <- as.factor(df$PPIUse)
df$BB <- as.factor(df$BB)
df$SSRI <- as.factor(df$SSRI)
df$Diuretics <- as.factor(df$Diuretics)
df$DayNight <- as.factor(df$DayNight)
df$Asterixis <- as.factor(df$Asterixis)
df$HCC <- as.factor(df$HCC)
df$HBL <- as.factor(df$HBL)
df$H30 <- as.factor(df$H30)
df$H3090 <- as.factor(df$H3090)
df$H90 <- as.factor(df$H90)
df$CH4BL <- as.factor(df$CH4BL)
df$CH430 <- as.factor(df$CH430)
df$CH43090 <- as.factor(df$CH43090)
df$CH490 <- as.factor(df$CH490)
df$Hydrogen <- as.factor(df$Hydrogen)
df$Methane <- as.factor(df$Methane)


# Hydrogen Collapsing the HBL groups -----
df$hbl_combined <- sapply(df$HBL, .GlobalEnv$combine_bl)

table(df$HBL, df$hbl_combined)

df$hbl_combined <- as.factor(df$hbl_combined)

# Hydrogen or Methane baseline -----

df$h2_ch4_0 <- mapply(.GlobalEnv$h2_me_bl_0, df$HBL, df$CH4BL)
df$h2_ch4_1 <- mapply(.GlobalEnv$h2_me_bl_1, df$HBL, df$CH4BL)
df$h2_ch4_2 <- mapply(.GlobalEnv$h2_me_bl_2, df$HBL, df$CH4BL)
df$h2_ch4_1_2 <- mapply(.GlobalEnv$h2_me_bl_1_2, df$HBL, df$CH4BL)

table(df$h2_ch4_0)
table(df$h2_ch4_1)
table(df$h2_ch4_2)
table(df$h2_ch4_1_2)

# Hydrogen Rise -----

table(df$Hydrogen)

df$h2_rise <- sapply(df$Hydrogen, .GlobalEnv$rise)

addmargins(table(df$h2_rise, df$Hydrogen, useNA = 'always'))

df$h2_rise_any <- sapply(df$Hydrogen, .GlobalEnv$rise_any)

# Methane Rise -----

df$ch4_rise <- sapply(df$Methane, .GlobalEnv$rise)
addmargins(table(df$ch4_rise, df$Methane, useNA = 'always'))

df$ch4_rise_any <- sapply(df$Methane, .GlobalEnv$rise_any)
addmargins(table(df$ch4_rise_any, df$Methane, useNA = 'always'))

# Hydrogen or Methane Rise ----

.GlobalEnv$rise_h2_ch4_any(22, NA)


df$rise_h2_ch4_0 <- mapply(.GlobalEnv$rise_h2_ch4_0, df$Hydrogen, df$Methane)
df$rise_h2_ch4_1 <- mapply(.GlobalEnv$rise_h2_ch4_1, df$Hydrogen, df$Methane)
df$rise_h2_ch4_2 <- mapply(.GlobalEnv$rise_h2_ch4_2, df$Hydrogen, df$Methane)
df$rise_h2_ch4_any <- mapply(.GlobalEnv$rise_h2_ch4_any, df$Hydrogen, df$Methane)

table(df$rise_h2_ch4_0)
table(df$rise_h2_ch4_1)
table(df$rise_h2_ch4_2)
table(df$rise_h2_ch4_any)

# Bin variables for chisq/fisher -----

df$lunchESS_high_low <- ifelse(df$lunchESS %in% c(0, 1), 0, 1)
df$markers_recode <- sapply(df$Markers, .GlobalEnv$recode_markders)
df$he_stage_recode <- sapply(df$HEStage, .GlobalEnv$overt_he_recode)

write.csv(df, file = 'data/working/cleaned_hebo.csv', row.names = FALSE)
