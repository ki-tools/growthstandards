#' Convert WHO z-scores/centiles to anthro measurements (generic)
#'
#' Get values of a specified measurement for a given WHO centile/z-score and growth standard pair (e.g. length vs. age) and sex over a specified grid
#'
#' @param x vector specifying the values of x over which to provide the centiles for y
#' @param p centile or vector of centiles at which to compute values (a number between 0 and 100 - 50 is median)
#' @param y_var y variable name (typically "htcm" or "wtkg") that specifies which variable of values should be returned for the specified value of q - see details
#' @param x_var x variable name (typically "agedays") - see details
#' @param sex "Male" or "Female"
#' @param data optional data frame that supplies any of the other variables provided to the function
#' @details for all supported pairings of \code{y_var} and \code{x_var} , type \code{names(who_coefs)}
#' @seealso \code{\link{who_value2centile}}, \code{\link{who_value2zscore}}
#' @examples
#' # median height vs. age for females
#' x <- seq(0, 365, by = 7)
#' med <- who_centile2value(x)
#' plot(x, med, xlab = "age in days", ylab = "median female height (cm)")
#'
#' # 99th percentile of weight vs. age for males from age 0 to 1461 days
#' dat <- data.frame(x = rep(seq(0, 1461, length = 100), 2),
#'   sex = rep(c("Male", "Female"), each = 100))
#' dat$p99 <- who_centile2value(x, p = 99, y_var = "wtkg", sex = sex, data = dat)
#' lattice::xyplot(kg2lb(p99) ~ days2years(x), groups = sex, data = dat,
#'   ylab = "99th percentile weight (pounds) for males",
#'   xlab = "age (years)", auto.key = TRUE)
#' @export
#' @rdname who_centile2value
who_centile2value <- function(x, p = 50, x_var = "agedays", y_var = "htcm",
  sex = "Female", data = NULL) {

  if (!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    p <- v_eval(substitute(p), try(p, silent = TRUE), data)
    x_var <- v_eval(substitute(x_var), try(x_var, silent = TRUE), data)
    y_var <- v_eval(substitute(y_var), try(y_var, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  dat <- data.frame(
    x = x,
    p = p,
    x_var = x_var,
    y_var = y_var,
    sex = sex,
    stringsAsFactors = FALSE
  )

  if (! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  centile2value_single_pars <- function(x, y, x_var, y_var, sex) {
    pair <- paste(y_var, x_var, sep = "_")
    check_pair(pair)

    coefs <- growthstandards::who_coefs[[pair]][[sex]]$data
    oob <- which(x < min(coefs$x) | x > max(coefs$x))

    # subset to neighborhood surrounding input
    idx <- get_coef_idx(x, coefs$x)
    coefs <- coefs[idx, , drop = FALSE] # nolint
    if (nrow(coefs) == 1) {
      coefs <- data.frame(y = y, coefs, row.names = NULL)
    } else {
      coefs <- data.frame(
        x = x,
        y = y,
        l = approx(coefs$x, coefs$l, x)$y,
        m = approx(coefs$x, coefs$m, x)$y,
        s = approx(coefs$x, coefs$s, x)$y)
    }

    res <- with(coefs, m * ((1 + qnorm(y / 100) * l * s)^(1 / l))) # nolint
    if (length(oob) > 0) {
      message("Observations with ", x_var, " value of ",
        paste(x[oob], collapse = ", "),
        " are outside the range of the standard. Setting to NA.")
      res[oob] <- NA
    }
    res

  }

  dat <- dat %>%
    dplyr::group_by(x_var, y_var, sex) %>%
    dplyr::mutate(res = centile2value_single_pars(x, p, x_var[1], y_var[1], sex[1]))

  dat$res
}

#' @param z z-score or vector of z-scores at which to compute values
#' @export
#' @rdname who_centile2value
who_zscore2value <- function(x, z = 0, y_var = "htcm", x_var = "agedays",
  sex = "Female", data = NULL) {

  if (!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    z <- v_eval(substitute(z), try(z, silent = TRUE), data)
    x_var <- v_eval(substitute(x_var), try(x_var, silent = TRUE), data)
    y_var <- v_eval(substitute(y_var), try(y_var, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  who_centile2value(p = 100 * pnorm(z), x = x, y_var = y_var, x_var = x_var, sex = sex)
}

#' Convert anthro measurements to WHO z-scores/centiles (generic)
#'
#' Compute z-scores or centiles with respect to the WHO growth standard for given values of x vs. y (typically x is "agedays" and y is a measure like "htcm").
#'
#' @param x value or vector of values that correspond to a measure defined by \code{x_var}
#' @param y value or vector of values that correspond to a measure defined by \code{y_var}
#' @param x_var x variable name (typically "agedays") - see details
#' @param y_var y variable name (typically "htcm" or "wtkg") - see details
#' @param sex "Male" or "Female"
#' @param data optional data frame that supplies any of the other variables provided to the function
#' @details for all supported pairings of \code{y_var} and \code{x_var} , type \code{names(who_coefs)}
#' @seealso \code{\link{who_centile2value}}, \code{\link{who_zscore2value}}
#' @examples
#' # z-scores
#' who_value2zscore(1670, in2cm(44))
#' who_value2zscore(1670, lb2kg(48), y_var = "wtkg")
#'
#' who_value2centile(1670, in2cm(44))
#' who_value2centile(1670, lb2kg(48), y_var = "wtkg")
#'
#' # add haz derived from WHO data and compare to that provided with data
#' cpp$haz <- who_value2zscore(x = agedays,  y = lencm, sex = sex, data = cpp)
#'
#' # note that you can also do it this way
#' #' cpp$haz <- who_value2zscore(cpp$agedays, cpp$lencm, sex = cpp$sex)
#' @export
#' @rdname who_value2zscore
who_value2zscore <- function(
  x, y,
  x_var = "agedays", y_var = "htcm",
  sex = "Female",
  data = NULL
) {

  if (!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    y <- v_eval(substitute(y), try(y, silent = TRUE), data)
    x_var <- v_eval(substitute(x_var), try(x_var, silent = TRUE), data)
    y_var <- v_eval(substitute(y_var), try(y_var, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  dat <- data.frame(
    x = x, y = y,
    x_var = x_var, y_var = y_var,
    sex = sex,
    stringsAsFactors = FALSE
  )

  if (! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  value2zscore_single_pars <- function(x, y, x_var, y_var, sex) {
    pair <- paste(y_var, x_var, sep = "_")
    check_pair(pair)

    coefs <- growthstandards::who_coefs[[pair]][[sex]]$data
    oob <- which(x < min(coefs$x) | x > max(coefs$x))

    # subset to neighborhood surrounding input
    idx <- get_coef_idx(x, coefs$x)
    coefs <- coefs[idx, , drop = FALSE] # nolint

    if (nrow(coefs) == 1) {
      coefs <- data.frame(y = y, coefs, row.names = NULL)
    } else {
      coefs <- data.frame(
        x = x,
        y = y,
        m = approx(coefs$x, coefs$m, x)$y,
        l = approx(coefs$x, coefs$l, x)$y,
        s = approx(coefs$x, coefs$s, x)$y
      )
    }

    res <- with(coefs, ((y / m)^l - 1) / (s * l)) # nolint

    if (length(oob) > 0) {
      message("Observations with ", x_var, " value of ",
        paste(x[oob], collapse = ", "),
        " are outside the range of the standard. Setting to NA.")
      res[oob] <- NA
    }
    res
  }

  dat <- dat %>%
    dplyr::group_by(x_var, y_var, sex) %>%
    dplyr::mutate(res = value2zscore_single_pars(x, y, x_var[1], y_var[1], sex[1]))

  dat$res
}

#' @export
#' @rdname who_value2zscore
who_value2centile <- function(
  x, y,
  x_var = "agedays",
  y_var = "htcm",
  sex = "Female",
  data = NULL
) {

  if (!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    y <- v_eval(substitute(y), try(y, silent = TRUE), data)
    x_var <- v_eval(substitute(x_var), try(x_var, silent = TRUE), data)
    y_var <- v_eval(substitute(y_var), try(y_var, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  pnorm(who_value2zscore(x = x, y = y, x_var = x_var, y_var = y_var, sex = sex)) * 100
}

## **2zscore
##---------------------------------------------------------

#' Convert anthro measurements to WHO z-scores/centiles
#'
#' @param agedays age in days
#' @param wtkg weight (kg) measurement(s) to convert
#' @param htcm height(cm) measurement(s) to convert
#' @param bmi body-mass index measurement(s) to convert
#' @param hcircm head circumference (cm) measurement(s) to convert
#' @param muaccm mid-upper arm circumference (cm) measurement(s) to convert
#' @param ssftmm subscalpular skinfold (mm) measurement(s) to convert
#' @param tsftmm triceps skinfold (mm) measurement(s) to convert
#' @param sex "Male" or "Female"
#' @export
#' @rdname who_var2zscore
#' @examples
#' cpp$haz <- who_value2zscore(x = agedays,  y = lencm, sex = sex, data = cpp)
who_wtkg2zscore <- function(agedays, wtkg, sex = "Female") {
  who_value2zscore(agedays, wtkg, x_var = "agedays", y_var = "wtkg", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_htcm2zscore <- function(agedays, htcm, sex = "Female") {
  who_value2zscore(agedays, htcm, x_var = "agedays", y_var = "htcm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_bmi2zscore <- function(agedays, bmi, sex = "Female") {
  who_value2zscore(agedays, bmi, x_var = "agedays", y_var = "bmi", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_hcircm2zscore <- function(agedays, hcircm, sex = "Female") {
  who_value2zscore(agedays, hcircm, x_var = "agedays", y_var = "hcircm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_muaccm2zscore <- function(agedays, muaccm, sex = "Female") {
  who_value2zscore(agedays, muaccm, x_var = "agedays", y_var = "muaccm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_ssftmm2zscore <- function(agedays, ssftmm, sex = "Female") {
  who_value2zscore(agedays, ssftmm, x_var = "agedays", y_var = "ssftmm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_tsftmm2zscore <- function(agedays, tsftmm, sex = "Female") {
  who_value2zscore(agedays, tsftmm, x_var = "agedays", y_var = "tsftmm", sex = sex)
}

## **2centile
##---------------------------------------------------------

#' @export
#' @rdname who_var2zscore
who_wtkg2centile <- function(agedays, wtkg, sex = "Female") {
  who_value2centile(agedays, wtkg, x_var = "agedays", y_var = "wtkg", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_htcm2centile <- function(agedays, htcm, sex = "Female") {
  who_value2centile(agedays, htcm, x_var = "agedays", y_var = "htcm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_bmi2centile <- function(agedays, bmi, sex = "Female") {
  who_value2centile(agedays, bmi, x_var = "agedays", y_var = "bmi", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_hcircm2centile <- function(agedays, hcircm, sex = "Female") {
  who_value2centile(agedays, hcircm, x_var = "agedays", y_var = "hcircm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_muaccm2centile <- function(agedays, muaccm, sex = "Female") {
  who_value2centile(agedays, muaccm, x_var = "agedays", y_var = "muaccm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_ssftmm2centile <- function(agedays, ssftmm, sex = "Female") {
  who_value2centile(agedays, ssftmm, x_var = "agedays", y_var = "ssftmm", sex = sex)
}

#' @export
#' @rdname who_var2zscore
who_tsftmm2centile <- function(agedays, tsftmm, sex = "Female") {
  who_value2centile(agedays, tsftmm, x_var = "agedays", y_var = "tsftmm", sex = sex)
}

## zscore2**
##---------------------------------------------------------

#' Convert WHO z-scores/centiles to anthro measurements
#'
#' @param agedays age in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param sex "Male" or "Female"
#' @examples
#' cpp$haz <- who_value2zscore(x = agedays,  y = lencm, sex = sex, data = cpp)
#' htcm <- who_zscore2htcm(cpp$agedays, cpp$haz, cpp$sex)
#' @export
#' @rdname who_zscore2var
who_zscore2htcm <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "htcm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2wtkg <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "wtkg", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2bmi <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "bmi", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2hcircm <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "hcircm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2muaccm <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "muaccm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2ssftmm <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "ssftmm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_zscore2tsftmm <- function(agedays, z = 0, sex = "Female") {
  who_zscore2value(agedays, z, x_var = "agedays", y_var = "tsftmm", sex = sex)
}

## centile2**
##---------------------------------------------------------

#' @export
#' @rdname who_zscore2var
who_centile2htcm <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "htcm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2wtkg <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "wtkg", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2bmi <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "bmi", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2hcircm <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "hcircm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2muaccm <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "muaccm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2ssftmm <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "ssftmm", sex = sex)
}

#' @export
#' @rdname who_zscore2var
who_centile2tsftmm <- function(agedays, p = 50, sex = "Female") {
  who_centile2value(agedays, p, x_var = "agedays", y_var = "tsftmm", sex = sex)
}


get_coef_idx <- function(x, coefx) {
  rng <- range(x, na.rm = TRUE)
  itr <- findInterval(coefx, rng)

  lower <- which(itr == 0)
  if (length(lower) == 0) {
    lower <- NULL
  } else {
    lower <- max(lower)
  }
  upper <- which(itr == 2)
  if (length(upper) == 0) {
    upper <- NULL
  } else {
    upper <- min(upper)
  }

  idx <- c(lower, which(itr == 1), upper)
  idx
}

# check_single <- function(par, par_name) {
#   if (length(par) != 1)
#     stop("currently can only get z-scores for one ", par, " at a time")
# }

check_pair <- function(pair) {
  if (! pair %in% c(
    "wtkg_agedays", "htcm_agedays", "bmi_agedays",
    "hcircm_agedays", "muaccm_agedays", "ssftmm_agedays",
    "tsftmm_agedays", "wtkg_htcm", "wtkg_lencm"
  ))
    stop("x and y pairings must be one of
x_var   | y_var
--------|--------
agedays | wtkg
agedays | htcm
agedays | bmi
agedays | hcircm
agedays | muaccm
agedays | ssftmm
agedays | tsftmm
htcm    | wtkg
lencm   | wtkg
")
}
