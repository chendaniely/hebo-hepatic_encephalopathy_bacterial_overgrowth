library(dplyr)

source('R/tables.R')
source('R/rise.R')

df <- read.csv('./data/working/cleaned_hebo.csv', stringsAsFactors = FALSE)


# Hydrogen Rise ----

tbl3_h2_rise <- df %>%
    group_by(h2_rise) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_rise)

tbl3_h2_rise_any <- df %>%
    group_by(h2_rise_any) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = h2_rise_any)

h2ch4_any <- rbind.data.frame(tbl3_h2_rise, tbl3_h2_rise_any)
h2ch4_any %>% as.matrix() %>% t() %>% write.csv('./output/tbl3_h2_transpose.csv')

# Methane Rise ----

tbl3_ch4_rise <- df %>%
    group_by(ch4_rise) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = ch4_rise)

tbl3_ch4_rise_any <- df %>%
    group_by(ch4_rise_any) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = ch4_rise_any)


h2ch4_any <- rbind.data.frame(tbl3_ch4_rise, tbl3_ch4_rise_any)
h2ch4_any %>% as.matrix() %>% t() %>% write.csv('./output/tbl3_ch4_transpose.csv')


# Hydrogen or Methane ----

h2ch4_any_0 <- df %>%
    group_by(rise_h2_ch4_0) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = rise_h2_ch4_0)

h2ch4_any_1 <- df %>%
    group_by(rise_h2_ch4_1) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = rise_h2_ch4_1)

h2ch4_any_2 <- df %>%
    group_by(rise_h2_ch4_2) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = rise_h2_ch4_2)

h2ch4_any_any <- df %>%
    group_by(rise_h2_ch4_any) %>%
    .GlobalEnv$my_summaries() %>%
    rename(group = rise_h2_ch4_any)


h2ch4_any <- rbind.data.frame(h2ch4_any_0, h2ch4_any_1, h2ch4_any_2, h2ch4_any_any)
h2ch4_any[c(-2, -4, -6, -8), ] %>%
    as.matrix() %>%
    t() %>%
    write.csv('./output/tbl3_h2_ch4_transpose.csv')
