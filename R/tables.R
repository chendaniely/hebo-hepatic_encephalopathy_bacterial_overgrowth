library(dplyr)

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
