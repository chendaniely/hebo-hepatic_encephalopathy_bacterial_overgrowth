library(ggplot2)

df <- read.csv('./data/working/cleaned_hebo.csv',stringsAsFactors = FALSE)
df


## esslunch -----

table(df$lunchESS, useNA = 'always')

ggplot(data = df, aes(x = lunchESS)) + geom_bar()

addmargins(table(df$lunchESS, df$h2_rise, useNA = 'always'))
addmargins(table(df$lunchESS, df$h2_rise_any, useNA = 'always'))


table(df$lunchESS_high_low)
addmargins(table(df$lunchESS_high_low, df$h2_rise, useNA = 'always'))

table(df$lunchESS_high_low)
addmargins(table(df$lunchESS_high_low, df$h2_rise_any, useNA = 'always'))

## markers -----

addmargins(table(df$Markers, df$h2_rise, useNA = 'always'))
addmargins(table(df$Markers, df$h2_rise_any, useNA = 'always'))

addmargins(table(df$markers_recode, df$h2_rise, useNA = 'always'))
addmargins(table(df$markers_recode, df$h2_rise_any, useNA = 'always'))


addmargins(table(df$markers_recode, df$h2_rise))

addmargins(table(df$markers_recode, df$h2_rise_any))

# he -----

table(df$HEStage, df$h2_rise, useNA = 'always')

ggplot(data = df, aes(x = HEStage)) + geom_bar()

table(df$HEStage, df$h2_rise_any, useNA = 'always')



table(df$HEStage, df$he_stage_recode)


addmargins(table(df$he_stage_recode, df$h2_rise_any))
