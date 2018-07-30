library(readxl)
library(dplyr)

df <- read_excel(here::here('data/HEBOnewworksheet5.21.xlsx'))

# replace 999 with NA missing value
df[df == 999] <- NA

my_summaries <- function(df) {
    df %>%
        summarize(n = n(),
                  avg_age = mean(Age, na.rm = TRUE),
                  std_age = sd(Age, na.rm = TRUE),
                  count_male = sum(Gender == 1, na.rm = TRUE),
                  pct_male = count_male / n,
                  etiology_0 = sum(Etiology == "0", na.rm = TRUE),
                  etiology_1 = sum(Etiology == "1", na.rm = TRUE),
                  etiology_2 = sum(Etiology == "2", na.rm = TRUE),
                  etiology_3 = sum(Etiology == "3", na.rm = TRUE),
                  #etiology_01 = sum(Etiology == "0,1", na.rm = TRUE),
                  #etiology_03 = sum(Etiology == "0,3", na.rm = TRUE),


                  varices_0 = sum(Varices == 0, na.rm = TRUE),
                  varices_1 = sum(Varices == 1, na.rm = TRUE),
                  varices_2 = sum(Varices == 2, na.rm = TRUE),
                  ascites_0 = sum(Ascites == 0, na.rm = TRUE),
                  ascites_1 = sum(Ascites == 1, na.rm = TRUE),
                  ascites_2 = sum(Ascites == 2, na.rm = TRUE),


                  sbp_0 = sum(SBP == 0, na.rm = TRUE),
                  sbp_1 = sum(SBP == 1, na.rm = TRUE),
                  he_0 = sum(HEStage == 0, na.rm = TRUE),
                  he_1 = sum(HEStage == 1, na.rm = TRUE),
                  he_2 = sum(HEStage == 2, na.rm = TRUE),


                  hcc_0 = sum(HCC == 0, na.rm = TRUE),
                  hcc_1 = sum(HCC == 1, na.rm = TRUE),
                  avg_na = mean(Na, na.rm = TRUE),
                  std_na = sd(Na, na.rm = TRUE),
                  avg_plt = mean(Platelet, na.rm = TRUE),
                  std_plt = sd(Platelet, na.rm = TRUE),
                  avg_albumin = mean(Albumin, na.rm = TRUE),
                  std_albumin = sd(Albumin, na.rm = TRUE),
                  avg_bun = mean(BUN, na.rm = TRUE),
                  std_bun = sd(BUN, na.rm = TRUE),
                  avg_alt = mean(ALT, na.rm = TRUE),
                  std_alt = sd(ALT, na.rm = TRUE),
                  avg_ast = mean(AST, na.rm = TRUE),
                  std_ast = sd(AST, na.rm = TRUE),
                  avg_tbili = mean(Tbili, na.rm = TRUE),
                  std_tbili = sd(Tbili, na.rm = TRUE),
                  avg_inr = mean(INR, na.rm = TRUE),
                  std_inr = sd(INR, na.rm = TRUE),
                  avg_cp_score = mean(Cpoints, na.rm = TRUE),
                  sd_cp_score = sd(Cpoints, na.rm = TRUE),
                  avg_meld = mean(MELD, na.rm = TRUE),
                  sd_meld = sd(MELD, na.rm = TRUE),
                  avg_na_meld = mean(NaMELD, na.rm = TRUE),
                  sd_na_meld = sd(NaMELD, na.rm = TRUE),
                  dm_0 = sum(DM == 0, na.rm = TRUE),
                  dm_1 = sum(DM == 1, na.rm = TRUE),
                  etoh_0 = sum(ETOH == 0, na.rm = TRUE),
                  etoh_1 = sum(ETOH == 1, na.rm = TRUE),


                  diuretics_0 = sum(Diuretics == 0, na.rm = TRUE),
                  diuretics_1 = sum(Diuretics == 1, na.rm = TRUE),
                  ppi_0 = sum(PPIUse == 0, na.rm = TRUE),
                  ppi_1 = sum(PPIUse == 1, na.rm = TRUE),
                  bb_0 = sum(BB == 0, na.rm = TRUE),
                  bb_1 = sum(BB == 1, na.rm = TRUE),


                  ess_0_8 = sum(ESS %in% 0:8, na.rm = TRUE),
                  ess_9_17 = sum(ESS %in% 9:17, na.rm = TRUE),
                  ess_18_24 = sum(ESS %in% 18:24, na.rm = TRUE),


                  ess_lunch_0 = sum(lunchESS == 0, na.rm = TRUE),
                  ess_lunch_1 = sum(lunchESS == 1, na.rm = TRUE),
                  ess_lunch_2 = sum(lunchESS == 2, na.rm = TRUE),
                  ess_lunch_3 = sum(lunchESS == 3, na.rm = TRUE),

                  markers_0 = sum(Markers == 0, na.rm = TRUE),
                  markers_1_5 = sum(Markers %in% 1:5, na.rm = TRUE),
                  markers_6_10 = sum(Markers %in% 6:10, na.rm = TRUE),
                  markers_g_10 = sum(Markers > 10, na.rm = TRUE),


                  avg_h2_90 = mean(HT90, na.rm = TRUE),
                  std_h2_90 = sd(HT90, na.rm = TRUE),
                  avg_h2_90_120 = mean(HT120, na.rm = TRUE),
                  std_h2_90_120 = mean(HT120, na.rm = TRUE),
                  avg_met_90 = mean(CH4T90, na.rm = TRUE),
                  std_met_90 = sd(CH4T90, na.rm = TRUE),
                  avg_met_90_120 = mean(CH4T120, na.rm = TRUE),
                  std_met_90_120 = sd(CH4T120, na.rm = TRUE)
        )
}

# Table 1 ----

# Hydrogen ----

tbl1_hbl <- df %>%
    group_by(HBL) %>%
    my_summaries()

tbl1_hbl_inPos <- df %>%
    filter(HBL %in% c(1, 2)) %>%
    my_summaries() %>%
    mutate(HBL = 'In + Pos')

tbl1_h2 <- rbind.data.frame(tbl1_hbl, tbl1_hbl_inPos)

tbl1_h2 %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_h2_transpose.csv')

# Methane ----

tbl1_mbl <- df %>%
    group_by(CH4BL) %>%
    my_summaries()

tbl1_mbl_inPos <- df %>%
    filter(CH4BL %in% c(1, 2)) %>%
    my_summaries() %>%
    mutate(CH4BL = 'In + Pos')

tbl1_ch4 <- rbind.data.frame(tbl1_mbl, tbl1_mbl_inPos)

tbl1_ch4 %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_ch4_transpose.csv')

# Hydrogen or Methane ----

h2_me_bl_0 <- function(h2, ch4){
    if (h2 == 0 | ch4 == 0) {
        return(0)
    } else {
        return(NA)
    }
}

h2_me_bl_1 <- function(h2, ch4){
    if (h2 == 1 | ch4 == 1) {
        return(1)
    } else {
        return(NA)
    }
}

h2_me_bl_2 <- function(h2, ch4){
    if (h2 == 2 | ch4 == 2) {
        return(2)
    } else {
        return(NA)
    }
}

h2_me_bl_1_2 <- function(h2, ch4){
    if (h2 %in% c(1, 2) | ch4 %in% c(1, 2)) {
        return(12)
    } else {
        return(NA)
    }
}

df$h2_ch4_0 <- mapply(h2_me_bl_0, df$HBL, df$CH4BL)
df$h2_ch4_1 <- mapply(h2_me_bl_1, df$HBL, df$CH4BL)
df$h2_ch4_2 <- mapply(h2_me_bl_2, df$HBL, df$CH4BL)
df$h2_ch4_1_2 <- mapply(h2_me_bl_1_2, df$HBL, df$CH4BL)

table(df$h2_ch4_0)
table(df$h2_ch4_1)
table(df$h2_ch4_2)
table(df$h2_ch4_1_2)

tbl1_h2_ch4_0 <- df %>%
    group_by(h2_ch4_0) %>%
    my_summaries() %>%
    rename(group = h2_ch4_0)

tbl1_h2_ch4_1 <- df %>%
    group_by(h2_ch4_1) %>%
    my_summaries() %>%
    rename(group = h2_ch4_1)

tbl1_h2_ch4_2 <- df %>%
    group_by(h2_ch4_2) %>%
    my_summaries() %>%
    rename(group = h2_ch4_2)

tbl1_h2_ch4_1_2 <- df %>%
    group_by(h2_ch4_1_2) %>%
    my_summaries() %>%
    rename(group = h2_ch4_1_2)


tbl1_h2_ch4_bl <- rbind.data.frame(tbl1_h2_ch4_0, tbl1_h2_ch4_1, tbl1_h2_ch4_2, tbl1_h2_ch4_1_2)
tbl1_h2_ch4_bl <- tbl1_h2_ch4_bl[!is.na(tbl1_h2_ch4_bl$group), ]

tbl1_h2_ch4_bl %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_h2_ch4_transpose.csv')

# Table 3 ----

# Hydrogen Rise ----

table(df$Hydrogen)

rise <- function(h2) {
    if (h2 %in% c(0, 10, 20)) {
        return('no rise')
    } else if (h2 %in% c(1, 11, 21)) {
        return('< 30')
    } else if (h2 %in% c(2, 12, 22)) {
        return('30 - 90')
    } else {
        return(NA)
    }
}

df$h2_rise <- sapply(df$Hydrogen, rise)

addmargins(table(df$h2_rise, df$Hydrogen, useNA = 'always'))

tbl3_h2_rise <- df %>%
    group_by(h2_rise) %>%
    my_summaries() %>%
    rename(group = h2_rise)



rise_any <- function(h2) {
    if (h2 %in% c(1, 2, 11, 12, 21, 22)) {
        return('0 - 90 (any)')
    } else if (h2 %in% c(0, 10, 20)) {
        return('no rise (any)')
    } else {
        return(NA)
    }
}

df$h2_rise_any <- sapply(df$Hydrogen, rise_any)

tbl3_h2_rise_any <- df %>%
    group_by(h2_rise_any) %>%
    my_summaries() %>%
    rename(group = h2_rise_any)

h2ch4_any <- rbind.data.frame(tbl3_h2_rise, tbl3_h2_rise_any)
h2ch4_any %>% as.matrix() %>% t() %>% write.csv('./output/tbl3_h2_transpose.csv')

# Methane Rise ----

df$ch4_rise <- sapply(df$Methane, rise)
addmargins(table(df$ch4_rise, df$Methane, useNA = 'always'))


tbl3_ch4_rise <- df %>%
    group_by(ch4_rise) %>%
    my_summaries() %>%
    rename(group = ch4_rise)

df$ch4_rise_any <- sapply(df$Methane, rise_any)
addmargins(table(df$ch4_rise_any, df$Methane, useNA = 'always'))


tbl3_ch4_rise_any <- df %>%
    group_by(ch4_rise_any) %>%
    my_summaries() %>%
    rename(group = ch4_rise_any)


h2ch4_any <- rbind.data.frame(tbl3_ch4_rise, tbl3_ch4_rise_any)
h2ch4_any %>% as.matrix() %>% t() %>% write.csv('./output/tbl3_ch4_transpose.csv')


# Hydrogen or Methane ----

rise_h2_ch4_0 <- function(h2, ch4) {
    h2_ch4 <- c(h2, ch4)
    if (any(h2_ch4 %in% c(0, 10, 20))) {
        return('no rise')
    } else {
        return(NA)
    }
}

rise_h2_ch4_1 <- function(h2, ch4) {
    h2_ch4 <- c(h2, ch4)
    if (any(h2_ch4 %in% c(1, 11, 21))) {
        return('< 30')
    } else {
        return(NA)
    }
}

rise_h2_ch4_2 <- function(h2, ch4) {
    h2_ch4 <- c(h2, ch4)
    if (any(h2_ch4 %in% c(2, 12, 22))) {
        return('30 - 90')
    } else {
        return(NA)
    }
}

rise_h2_ch4_any <- function(h2, ch4) {
    h2_ch4 <- c(h2, ch4)
    if (any(h2_ch4 %in% c(1, 2, 11, 12, 21, 22))) {
        return('0 - 90 (any)')
    } else {
        return(NA)
    }
}

rise_h2_ch4_any(22, NA)


df$rise_h2_ch4_0 <- mapply(rise_h2_ch4_0, df$Hydrogen, df$Methane)
df$rise_h2_ch4_1 <- mapply(rise_h2_ch4_1, df$Hydrogen, df$Methane)
df$rise_h2_ch4_2 <- mapply(rise_h2_ch4_2, df$Hydrogen, df$Methane)
df$rise_h2_ch4_any <- mapply(rise_h2_ch4_any, df$Hydrogen, df$Methane)

table(df$rise_h2_ch4_0)
table(df$rise_h2_ch4_1)
table(df$rise_h2_ch4_2)
table(df$rise_h2_ch4_any)

h2ch4_any_0 <- df %>%
    group_by(rise_h2_ch4_0) %>%
    my_summaries() %>%
    rename(group = rise_h2_ch4_0)

h2ch4_any_1 <- df %>%
    group_by(rise_h2_ch4_1) %>%
    my_summaries() %>%
    rename(group = rise_h2_ch4_1)

h2ch4_any_2 <- df %>%
    group_by(rise_h2_ch4_2) %>%
    my_summaries() %>%
    rename(group = rise_h2_ch4_2)

h2ch4_any_any <- df %>%
    group_by(rise_h2_ch4_any) %>%
    my_summaries() %>%
    rename(group = rise_h2_ch4_any)


h2ch4_any <- rbind.data.frame(h2ch4_any_0, h2ch4_any_1, h2ch4_any_2, h2ch4_any_any)
h2ch4_any[c(-2, -4, -6, -8), ] %>% as.matrix() %>% t() %>% write.csv('./output/tbl3_h2_ch4_transpose.csv')

