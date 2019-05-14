#' Convert mother's gestational weight gain (GWG) measurements to INTERGROWTH z-scores/centiles
#'
#' @param gagedays gestational age(s) in days at which gestational weight gain was measured (must be same length as \code{val})
#' @param val value(s) of the gestational weight gain (kg gained since pregnancy) measurement(s) to convert
#' @param bmi optional value(s) of mother's pre-pregnancy BMI (kg / m^2) to check validity of GWG standard
#' @export
#' @references Gestational weight gain standards based on women enrolled in the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective longitudinal cohort study
#' Ismail, Leila Cheikh et al.
#' BMJ 2016;352:i555
#' @examples
#' # get z-score and centile for mother at 17 weeks with 2 kg gestational weight gain
#' iggwg_value2zscore(17 * 7, 2)
#' iggwg_value2centile(17 * 7, 2)
#' @rdname iggwg_value2zscore
#' @export
iggwg_value2zscore <- function(gagedays, val, bmi = NULL) {
  if (length(gagedays) != length(val))
    stop("gagedays and val must have the same length", call. = FALSE)

  if (!is.null(bmi)) {
    if (length(bmi) == 1)
      bmi <- rep(bmi, length(gagedays))
    if (length(bmi) != length(val))
      stop("bmi must be length 1 or the length of gagedays and val",
        call. = FALSE)
  } else {
    message("Argument 'bmi' not supplied... presuming valid pre-pregnancy BMI.")
  }

  gaweeks <- gagedays / 7
  val <- replace(val, gaweeks < 14 | gaweeks > 40, NA)
  val <- replace(val, bmi < 18.5 | bmi >= 25, NA)

  mu <- (1.382972 - 56.14743 * (gaweeks ^ (-2)) + 0.2787683 * gaweeks ^ 0.5)
  sigma <- 0.2501993731 + 142.4297879 * (gaweeks ^ (-2)) -
    (61.45345 * (gaweeks ^ (-2)) * log(gaweeks))

  (log(val + 8.75) - mu) / sigma
}

#' @rdname iggwg_value2zscore
#' @export
iggwg_value2centile <- function(gagedays, val, bmi = NULL) {
  pnorm(iggwg_value2zscore(gagedays, val, bmi)) * 100
}

#' Convert INTERGROWTH z-scores/centiles to gestational weight gains (GWG)
#'
#' @param gagedays gestational age(s) in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param bmi optional value(s) of mother's pre-pregnancy BMI (kg / m^2) to check validity of GWG standard
#' @export
#' @references Gestational weight gain standards based on women enrolled in the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project: a prospective longitudinal cohort study
#' Ismail, Leila Cheikh et al.
#' BMJ 2016;352:i555
#' @examples
#' # get value for median GWG for mother at 40 gestational weeks
#' iggwg_centile2value(40 * 7, 50)
#' @rdname iggwg_zscore2value
#' @export
iggwg_zscore2value <- function(gagedays, z = 0, bmi = NULL) {
  if (length(z) == 1)
    z <- rep(z, length(gagedays))

  if (!is.null(bmi)) {
    if (length(bmi) == 1)
      bmi <- rep(bmi, length(gagedays))
    if (length(bmi) != length(z))
      stop("bmi must be length 1 or the length of gagedays and val",
        call. = FALSE)
  } else {
    message("Argument 'bmi' not supplied... presuming valid pre-pregnancy BMI.")
  }

  gaweeks <- gagedays / 7
  z <- replace(z, gaweeks < 14 | gaweeks > 40, NA)
  z <- replace(z, bmi < 18.5 | bmi >= 25, NA)

  mu <- (1.382972 - 56.14743 * gaweeks ^ (-2) + 0.2787683 * gaweeks ^ 0.5)
  sigma <- 0.2501993731 + 142.4297879 * (gaweeks ^ (-2)) -
    (61.45345 * (gaweeks ^ (-2)) * log(gaweeks))

  exp(z * sigma + mu) - 8.75
}

#' @rdname iggwg_zscore2value
#' @export
iggwg_centile2value <- function(gagedays, p = 50, bmi = NULL) {
  iggwg_zscore2value(gagedays, qnorm(p / 100), bmi)
}
