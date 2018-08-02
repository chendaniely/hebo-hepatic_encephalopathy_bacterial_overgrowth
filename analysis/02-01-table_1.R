library(dplyr)

source('R/tables.R')
source('R/recode.R')

df <- read.csv('./data/working/cleaned_hebo.csv', stringsAsFactors = FALSE)


# Hydrogen ----

tbl1_hbl <- df %>%
    group_by(HBL) %>%
    .GlobalEnv$my_summaries()

tbl1_hbl_inPos <- df %>%
    filter(HBL %in% c(1, 2)) %>%
    .GlobalEnv$my_summaries() %>%
    mutate(HBL = 'In + Pos')

tbl1_h2 <- rbind.data.frame(tbl1_hbl, tbl1_hbl_inPos)

tbl1_h2 %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_h2_transpose.csv')

# Methane ----

tbl1_mbl <- df %>%
    group_by(CH4BL) %>%
    .GlobalEnv$my_summaries()

tbl1_mbl_inPos <- df %>%
    filter(CH4BL %in% c(1, 2)) %>%
    .GlobalEnv$my_summaries() %>%
    mutate(CH4BL = 'In + Pos')

tbl1_ch4 <- rbind.data.frame(tbl1_mbl, tbl1_mbl_inPos)

tbl1_ch4 %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_ch4_transpose.csv')

tbl1_h2_ch4_0 <- df %>%
    group_by(h2_ch4_0) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_ch4_0)

tbl1_h2_ch4_1 <- df %>%
    group_by(h2_ch4_1) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_ch4_1)

tbl1_h2_ch4_2 <- df %>%
    group_by(h2_ch4_2) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_ch4_2)

tbl1_h2_ch4_1_2 <- df %>%
    group_by(h2_ch4_1_2) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_ch4_1_2)


tbl1_h2_ch4_bl <- rbind.data.frame(tbl1_h2_ch4_0, tbl1_h2_ch4_1, tbl1_h2_ch4_2, tbl1_h2_ch4_1_2)
tbl1_h2_ch4_bl <- tbl1_h2_ch4_bl[!is.na(tbl1_h2_ch4_bl$group), ]

tbl1_h2_ch4_bl %>% as.matrix() %>% t() %>% write.csv('./output/tbl1_h2_ch4_transpose.csv')
