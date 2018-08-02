#' Combine baselines for hydrogen and methane
#'
#' baselines for hydrogen and methane were designated as 0
#' subsets: 00 (no rise), 01 (rise before 30m), and 02 (rise between 30 and 90 min)
#'
#' intermediate baselines for hydrogen and methan were designated as 1
#' subsets: 10 (no rise), 11 (rise before 30m), and 12 (rise between 30 and 90 min)
#'
#' positive baselines for hydrogen and methan were designated as 2
#' subsets: 20 (no rise), 21 (rise before 30m), and 22 (rise between 30 and 90 min)
#'
#' R will read in the values as numberic, so the leading 0 will be dropped
combine_bl <- function(bl) {
    if (bl == 0) {
        return(0)
    } else if (bl %in% c(1, 2)) {
        return(12)
    } else {
        return(NA)
    }
}


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

recode_markders <- function(marker) {
    if (is.na(marker)) {
        return(NA)
    } else if (marker == 0) {
        return(0)
    } else if (marker %in% 1:5) {
        return(1)
    } else if (marker %in% 6:10) {
        return(2)
    } else if (marker > 10) {
        return(3)
    } else {
        stop('unknown marker')
    }
}

overt_he_recode <- function(he) {
    if (is.na(he)) {
        return(NA)
    } else if (he %in% 0:1) {
        return(0)
    } else if (he %in% 2) {
        return(1)
    } else {
        stop('unknown he')
    }
}
