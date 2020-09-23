#' Convert postnatal growth measurements for preterm infants to INTERGROWTH z-scores/centiles (generic)
#'
#' @param pmagedays postmenstrual age in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm")
#' @param sex "Male" or "Female"
#' @references Postnatal growth standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.
#' Villar J, Giuliani F, Bhutta ZA, et al.
#' Lancet Glob Health 2015; 3: e681–91.
#' @examples
#' # get median wtkg of Females from 27 to 64 weeks postmenstrual age
#' igprepost_centile2value(27:64 * 7, 50, var = "wtkg", sex = "Female")
#' @export
igprepost_centile2value <- function(pmagedays, p = 50, var = "lencm", sex = "Female") {
  dat <- data.frame(x = pmagedays, p = p, var = var, sex = sex,
    stringsAsFactors = FALSE)

  if (! all(unique(dat$sex[!is.na(dat$sex)]) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  if (! all(var %in% c("lencm", "wtkg", "hcircm")))
    stop("'var' must be one of 'lencm', 'wtkg', 'hcircm'")

  ig_centile2value_single_pars <- function(x, y, var, sex) {
    x <- x / 7

    tr <- exp
    if (var == "wtkg") {
      mn <- 2.591277 - 0.01155 * x ^ 0.5 - 2201.705 * x ^ (-2) +
        0.0911639 * (sex == "Male")
      sd <- 0.1470258 + 505.92394 * x ^ (-2) - 140.0576 * x ^ (-2) * log(x)
    } else if (var == "lencm") {
      mn <- 4.136244 - 547.0018 * x ^ (-2) + 0.0026066 * x +
        0.0314961 * (sex == "Male")
      sd <- 0.050489 + 310.44761 * x ^ (-2) - 90.0742 * x ^ (-2) * log(x)
    } else if (var == "hcircm") {
      mn <- 55.53617 - 852.0059 * x ^ (-1) + 0.7957903 * (sex == "Male")
      sd <- 3.0582292 + 3910.05 * x ^ (-2) - 180.5625 * x ^ (-1)
      tr <- identity
    }

    tr(mn + (qnorm(y / 100) * sd))
  }
  dat <- data.frame(dat %>%
    dplyr::group_by(var, sex) %>%
    dplyr::mutate(res = ig_centile2value_single_pars(x, p, var[1], sex[1])))

  dat$res[dat$x < 24 * 7] <- NA
  dat$res[dat$x > 64 * 7 + 6] <- NA

  dat$res
}


#' Convert z-scores to postnatal growth measurements for preterm infants (generic)
#'
#' @rdname igprepost_zscore2value
#' @export
igprepost_zscore2value <- function(pmagedays, z = 0, var = "lencm", sex = "Female") {
  igprepost_centile2value(pmagedays, p = 100 * pnorm(z), var = var, sex = sex)
}

#' Convert postnatal growth measurements for preterm infants to INTERGROWTH z-scores/centiles (generic)
#'
#' @param pmagedays postmenstrual age in days
#' @param val the value(s) of the anthro measurement to convert
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm")
#' @param sex "Male" or "Female"
#' @references Postnatal growth standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.
#' Villar J, Giuliani F, Bhutta ZA, et al.
#' Lancet Glob Health 2015; 3: e681–91.
#' @examples
#' # get centile of 0.99kg 27 week postmenstrual age Male and 0.91kg 27 week postmenstrual age Female
#' igprepost_value2centile(27 * 7, c(0.99, 0.91), var = "wtkg",
#'   sex = c("Male", "Female"))
#' @rdname igprepost_value2zscore
#' @export
igprepost_value2centile <- function(pmagedays, val, var = "lencm", sex = "Female") {

  dat <- data.frame(x = pmagedays, y = val, var = var, sex = sex, stringsAsFactors = FALSE)

  if (! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  if (! all(var %in% c("lencm", "wtkg", "hcircm")))
    stop("'var' must be one of 'lencm', 'wtkg', 'hcircm'")

  ig_value2centile_single_pars <- function(x, y, var, sex) {
    x <- x / 7

    tr <- log
    if (var == "wtkg") {
      mn <- 2.591277 - 0.01155 * x ^ 0.5 - 2201.705 * x ^ (-2) +
        0.0911639 * (sex == "Male")
      sd <- 0.1470258 + 505.92394 * x ^ (-2) - 140.0576 * x ^ (-2) * log(x)
    } else if (var == "lencm") {
      mn <- 4.136244 - 547.0018 * x ^ (-2) + 0.0026066 * x +
        0.0314961 * (sex == "Male")
      sd <- 0.050489 + 310.44761 * x ^ (-2) - 90.0742 * x ^ (-2) * log(x)
    } else if (var == "hcircm") {
      mn <- 55.53617 - 852.0059 * x ^ (-1) + 0.7957903 * (sex == "Male")
      sd <- 3.0582292 + 3910.05 * x ^ (-2) - 180.5625 * x ^ (-1)
      tr <- identity
    }

    z <- (tr(y) - mn) / sd
    pnorm(z) * 100
  }

  dat <- dat %>%
    dplyr::group_by(var, sex) %>%
    dplyr::mutate(res = ig_value2centile_single_pars(x, y, var[1], sex[1]))

  dat$res[dat$x < 24 * 7] <- NA
  dat$res[dat$x > 64 * 7 + 6] <- NA

  dat$res
}

#' @rdname igprepost_value2zscore
#' @export
igprepost_value2zscore <- function(pmagedays, val, var = "lencm", sex = "Female") {
  qnorm(igprepost_value2centile(pmagedays, val, var = var, sex = sex) / 100)
}


## **2zscore
##---------------------------------------------------------

#' Convert postnatal growth measurements for preterm infants to INTERGROWTH z-scores/centiles
#'
#' @param pmagedays postmenstrual age in days
#' @param wtkg weight (kg) measurement(s) to convert
#' @param lencm length(cm) measurement(s) to convert
#' @param hcircm head circumference (cm) measurement(s) to convert
#' @param wlr weight-length ratio values(s) to convert
#' @param sex "Male" or "Female"
#' @references Postnatal growth standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.
#' Villar J, Giuliani F, Bhutta ZA, et al.
#' Lancet Glob Health 2015; 3: e681–91.
#' @examples
#' igprepost_lencm2zscore(64 * 7, 64.68, sex = "Female")
#' @rdname igprepost_var2zscore
#' @export
igprepost_lencm2zscore <- function(pmagedays, lencm, sex = "Female") {
  igprepost_value2zscore(pmagedays, lencm, var = "lencm", sex = sex)
}

#' @rdname igprepost_var2zscore
#' @export
igprepost_wtkg2zscore <- function(pmagedays, wtkg, sex = "Female") {
  igprepost_value2zscore(pmagedays, wtkg, var = "wtkg", sex = sex)
}

#' @rdname igprepost_var2zscore
#' @export
igprepost_hcircm2zscore <- function(pmagedays, hcircm, sex = "Female") {
  igprepost_value2zscore(pmagedays, hcircm, var = "hcircm", sex = sex)
}

## **2centile
##---------------------------------------------------------

#' @rdname igprepost_var2zscore
#' @export
igprepost_lencm2centile <- function(pmagedays, lencm, sex = "Female") {
  igprepost_value2centile(pmagedays, lencm, var = "lencm", sex = sex)
}

#' @rdname igprepost_var2zscore
#' @export
igprepost_wtkg2centile <- function(pmagedays, wtkg, sex = "Female") {
  igprepost_value2centile(pmagedays, wtkg, var = "wtkg", sex = sex)
}

#' @rdname igprepost_var2zscore
#' @export
igprepost_hcircm2centile <- function(pmagedays, hcircm, sex = "Female") {
  igprepost_value2centile(pmagedays, hcircm, var = "hcircm", sex = sex)
}

## zscore2**
##---------------------------------------------------------

#' Convert INTERGROWTH z-scores/centiles to postnatal growth measurements for preterm infants
#'
#' @param pmagedays postmenstrual age in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param sex "Male" or "Female"
#' @references Postnatal growth standards for preterm infants: the Preterm Postnatal Follow-up Study of the INTERGROWTH-21st Project.
#' Villar J, Giuliani F, Bhutta ZA, et al.
#' Lancet Glob Health 2015; 3: e681–91.
#' @examples
#' igprepost_zscore2lencm(64 * 7, 0, sex = "Female")
#' 
#' d <- expand.grid(centile = c(3, 50, 97), pmage = c(27:64) * 7)
#' d$value <- igprepost_centile2lencm(d$pmage, d$centile, sex = "Male")
#' lattice::xyplot(value ~ pmage / 7, groups = centile, data = d, type = "l")
#' @rdname igprepost_zscore2var
#' @note For gestational ages between 24 and 33 weeks, the INTERGROWTH very early preterm standard is used.
#' @export
igprepost_zscore2lencm <- function(pmagedays, z = 0, sex = "Female") {
  igprepost_zscore2value(pmagedays, z, var = "lencm", sex = sex)
}

#' @rdname igprepost_zscore2var
#' @export
igprepost_zscore2wtkg <- function(pmagedays, z = 0, sex = "Female") {
  igprepost_zscore2value(pmagedays, z, var = "wtkg", sex = sex)
}

#' @rdname igprepost_zscore2var
#' @export
igprepost_zscore2hcircm <- function(pmagedays, z = 0, sex = "Female") {
  igprepost_zscore2value(pmagedays, z, var = "hcircm", sex = sex)
}

## centile2**
##---------------------------------------------------------

#' @rdname igprepost_zscore2var
#' @export
igprepost_centile2lencm <- function(pmagedays, p = 50, sex = "Female") {
  igprepost_centile2value(pmagedays, p, var = "lencm", sex = sex)
}

#' @rdname igprepost_zscore2var
#' @export
igprepost_centile2wtkg <- function(pmagedays, p = 50, sex = "Female") {
  igprepost_centile2value(pmagedays, p, var = "wtkg", sex = sex)
}

#' @rdname igprepost_zscore2var
#' @export
igprepost_centile2hcircm <- function(pmagedays, p = 50, sex = "Female") {
  igprepost_centile2value(pmagedays, p, var = "hcircm", sex = sex)
}
