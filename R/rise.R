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

rise_any <- function(h2) {
    if (h2 %in% c(1, 2, 11, 12, 21, 22)) {
        return('0 - 90 (any)')
    } else if (h2 %in% c(0, 10, 20)) {
        return('no rise (any)')
    } else {
        return(NA)
    }
}

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
