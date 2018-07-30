library(readxl)
library(dplyr)

df <- read_excel(here::here('data/HEBOnewworksheet5.21.xlsx'))

# replace 999 with NA missing value
df[df == 999] <- NA

combine_bl <- function(bl) {
    if (bl == 0) {
        return(0)
    } else if (bl %in% c(1, 2)) {
        return(12)
    } else {
        return(NA)
    }
}


df$hbl_combined <- sapply(df$HBL, combine_bl)

table(df$HBL, df$hbl_combined)

my_aov <- function(var, group) {
    form <- sprintf('%s ~ %s', group, var)
    f <- as.formula(form)
    mod <- aov(f, df)
    summary(mod)
}

lapply(names(df), my_aov, group = 'HBL')
