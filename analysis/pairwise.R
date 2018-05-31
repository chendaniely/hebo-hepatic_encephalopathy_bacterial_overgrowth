library(readxl)
df <- read_excel(here::here('data/HEBOnewworksheet5.21.xlsx'))

# replace 999 with NA missing value
df[df == 999] <- NA

recode_any_rise <- function(h2_me_value) {
    if (h2_me_value %in% c(0, 10, 20)) {
        return('no rise')
    } else if (h2_me_value %in% c(1, 2, 11, 12, 21, 22)) {
        return('some rise')
    } else {
        return(NA)
    }
}

df$any_h2_rise <- sapply(df$Hydrogen, recode_any_rise)
df$any_me_rise <- sapply(df$Methane, recode_any_rise)
df$any_h2me_rise <- ifelse(df$any_h2_rise == 'some rise' | df$any_me_rise == 'some rise',
                           'some rise', 'no rise')


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
df$any_h2_rise <- as.factor(df$any_h2_rise)
df$any_me_rise <- as.factor(df$any_me_rise)
df$any_h2me_rise <- as.factor(df$any_h2me_rise)

col_classes <- sapply(df, class)
col_factors <- col_classes[col_classes == 'factor']
col_factors_names <- names(col_factors)
col_factors_names

df <- na.omit(df)

for (colname in col_factors_names){
    print(colname)
    print(df[, colname])
    print(df[, 'any_h2me_rise'])
    fisher.test(data.frame(df[, 'any_h2me_rise'], df[, colname, drop = TRUE]))
}


v <- fisher.test(df$any_h2me_rise, df$Etiology)
v$p.value







fish <- list(
    fisher.test(df$any_h2me_rise, df$Etiology),
    fisher.test(df$any_h2me_rise, df$Lactulose),
    fisher.test(df$any_h2me_rise, df$Gender),
    fisher.test(df$any_h2me_rise, df$Depression),
    fisher.test(df$any_h2me_rise, df$SBP),
    fisher.test(df$any_h2me_rise, df$DM),
    fisher.test(df$any_h2me_rise, df$ETOH),
    fisher.test(df$any_h2me_rise, df$Cclass),
    fisher.test(df$any_h2me_rise, df$HEStage),
    fisher.test(df$any_h2me_rise, df$Ascites),
    fisher.test(df$any_h2me_rise, df$EVB),
    fisher.test(df$any_h2me_rise, df$Varices),
    fisher.test(df$any_h2me_rise, df$PPIUse),
    fisher.test(df$any_h2me_rise, df$BB),
    fisher.test(df$any_h2me_rise, df$SSRI),
    fisher.test(df$any_h2me_rise, df$Diuretics),
    fisher.test(df$any_h2me_rise, df$DayNight),
    fisher.test(df$any_h2me_rise, df$Asterixis),
    fisher.test(df$any_h2me_rise, df$HCC),
    fisher.test(df$any_h2me_rise, df$HBL),
    fisher.test(df$any_h2me_rise, df$H30),
    fisher.test(df$any_h2me_rise, df$H3090),
    fisher.test(df$any_h2me_rise, df$H90),
    fisher.test(df$any_h2me_rise, df$CH4BL),
    fisher.test(df$any_h2me_rise, df$CH430),
    fisher.test(df$any_h2me_rise, df$CH43090),
    fisher.test(df$any_h2me_rise, df$CH490),
    fisher.test(df$any_h2me_rise, df$Hydrogen),
    fisher.test(df$any_h2me_rise, df$Methane),
    fisher.test(df$any_h2me_rise, df$any_h2_rise),
    fisher.test(df$any_h2me_rise, df$any_me_rise),
    fisher.test(df$any_h2me_rise, df$any_h2me_rise)
)


fish

ps <- lapply(fish, '[[', 1)

ps <- unlist(ps)
ps

ps < .05


sig <- which(ps <= .1)

fish[sig]





fisher.test(df$Cclass, df$Diuretics)

model <- glm(any_h2me_rise ~ Diuretics, family = binomial(link = 'logit'), data = df)
summary(model)









col_classes <- sapply(df, class)
col_numeric <- col_classes[col_classes == 'numeric']
col_numeric_names <- names(col_numeric)
col_numeric_names


ts <- lapply(col_numeric_names, function(x){pairwise.t.test(df[, x, drop=TRUE], df$any_h2me_rise)})

tsig <- lapply(ts, '[[', 3) < .05


ts[tsig]
