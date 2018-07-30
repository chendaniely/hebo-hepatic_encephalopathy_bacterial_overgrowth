library(readxl)
library(nnet)
library(glmnet)
library(useful)

options(scipen = 999)

# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

df <- read_excel(here::here('data/HEBOnewworksheet5.21.xlsx'))

# replace 999 with NA missing value
df[df == 999] <- NA
# df[df$Etiology == 2, 'Etiology'] <- NA
# df[df$Cclass == 2, 'Cclass'] <- NA

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

df$any_h2me_rise <- ifelse(df$any_h2_rise == 'some rise' | df$any_me_rise == 'some rise', 'some rise', 'no rise')

model <- glm(as.factor(any_me_rise) ~ as.factor(Gender) + Age + MELD + NaMELD, family = binomial(link = 'logit'), data = df)
summary(model)
broom::tidy(model)

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

fct_table <- lapply(col_factors_names, function(x){table(df[, x])})
names(fct_table) <- col_factors_names

fct_table


fct_df <- df[, col_factors_names]
fct_df <- na.omit(fct_df)

fisher.test(fct_df)

drop_cols <- c('Patient', 'SBP', 'Asterixis', 'Etiology', 'Markers')

dim(df)
df <- df[, !names(df) %in% drop_cols]
dim(df)


# fisher.test(df)


# multinomial logistic regression
mod <- multinom(HEStage ~ ., data = df)
coefs <- broom::tidy(mod)
coefs$estimate <- round(coefs$estimate, 2)
coefs$p.value <- round(coefs$p.value, 2)
coefs$std.error <- round(coefs$std.error, 2)
coefs$statistic <- round(coefs$statistic, 2)
coefs

coefs[coefs$p.value < .1, ]


x <- useful::build.x(any_me_rise ~ ., data = df, contrasts = FALSE)
y <- useful::build.y(any_me_rise ~ ., data = df)

cvfit <- cv.glmnet(x, y, family = "binomial", parallel = TRUE, nfolds = 5)
plot(cvfit)

cvfit$lambda.1se
cvfit$lambda.min



fit = glmnet(x, y, family = "binomial", lambda = cvfit$lambda.1se)
plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")

broom::tidy(fit)

