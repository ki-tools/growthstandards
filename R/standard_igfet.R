#' Convert fetal ultrasound measurements to INTERGROWTH z-scores/centiles (generic)
#'
#' @param gagedays gestational age in days
#' @param val the value(s) of the anthro measurement to convert
#' @param var the name of the measurement to convert ("hccm", "bpdcm", "ofdcm", "accm", "flcm")
#' @export
#' @references International standards for fetal growth based on serial ultrasound measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project
#' Papageorghiou, Aris T et al.
#' The Lancet, Volume 384, Issue 9946, 869-879
#' @examples
#' # get centile for child at 100 gestational days with 11 cm head circumference
#' igfet_hccm2centile(100, 11)
#' @rdname igfet_value2zscore
#' @export
igfet_value2zscore <- function(gagedays, val,
  var = c("hccm", "bpdcm", "ofdcm", "accm", "flcm")) {

  gaweeks <- gagedays / 7
  val <- replace(val, gaweeks < 14 | gaweeks > 40, NA)
  var <- match.arg(var)
  pars <- get_igfet_pars(var, gaweeks)

  (val - pars$mean) / pars$sd
}

#' @rdname igfet_value2zscore
#' @export
igfet_value2centile <- function(gagedays, val,
  var = c("hccm", "bpdcm", "ofdcm", "accm", "flcm")) {

  pnorm(igfet_value2zscore(gagedays, val, var)) * 100
}


#' Convert INTERGROWTH z-scores/centiles to fetal ultrasound measurements (generic)
#'
#' @param gagedays gestational age in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param var the name of the measurement to convert ("hccm", "bpdcm", "ofdcm", "accm", "flcm")
#' @export
#' @references International standards for fetal growth based on serial ultrasound measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project
#' Papageorghiou, Aris T et al.
#' The Lancet, Volume 384, Issue 9946, 869-879
#' @examples
#' # get value for median head circumference for child at 100 gestational days
#' igfet_centile2value(100, 50, var = "hccm")
#' @rdname igfet_zscore2value
#' @export
igfet_zscore2value <- function(gagedays, z = 0,
  var = c("hccm", "bpdcm", "ofdcm", "accm", "flcm")) {

  if (length(z) == 1)
    z <- rep(z, length(gagedays))

  gaweeks <- gagedays / 7
  z <- replace(z, gaweeks < 14 | gaweeks > 40, NA)
  var <- match.arg(var)
  pars <- get_igfet_pars(var, gaweeks)

  z * pars$sd + pars$mean
}

#' @rdname igfet_zscore2value
#' @export
igfet_centile2value <- function(gagedays, p = 50,
  var = c("hccm", "bpdcm", "ofdcm", "accm", "flcm")) {

  igfet_zscore2value(gagedays, qnorm(p / 100), var)
}


## **2zscore
##---------------------------------------------------------

#' Convert fetal ultrasound measurements to INTERGROWTH z-scores/centiles
#'
#' @param gagedays gestational age in days
#' @param hccm head circumference (cm) measurement(s) to convert
#' @param bpdcm biparietel diameter (cm) measurement(s) to convert
#' @param ofdcm occipito-frontal diameter (cm) measurement(s) to convert
#' @param accm abdominal circumference (cm) measurement(s) to convert
#' @param flcm femur length (cm) measurement(s) to convert
#' @references International standards for fetal growth based on serial ultrasound measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project
#' Papageorghiou, Aris T et al.
#' The Lancet, Volume 384, Issue 9946, 869-879
#' @examples
#' # get centile for child at 100 gestational days with 11 cm head circumference
#' igfet_hccm2centile(100, 11)
#' @rdname igfet_var2zscore
#' @export
igfet_hccm2zscore <- function(gagedays, hccm) {
  igfet_value2zscore(gagedays, hccm, var = "hc")
}

#' @rdname igfet_var2zscore
#' @export
igfet_bpdcm2zscore <- function(gagedays, bpdcm) {
  igfet_value2zscore(gagedays, bpdcm, var = "bpd")
}

#' @rdname igfet_var2zscore
#' @export
igfet_ofdcm2zscore <- function(gagedays, ofdcm) {
  igfet_value2zscore(gagedays, ofdcm, var = "ofd")
}

#' @rdname igfet_var2zscore
#' @export
igfet_accm2zscore <- function(gagedays, accm) {
  igfet_value2zscore(gagedays, accm, var = "ac")
}

#' @rdname igfet_var2zscore
#' @export
igfet_flcm2zscore <- function(gagedays, flcm) {
  igfet_value2zscore(gagedays, flcm, var = "fl")
}

## **2centile
##---------------------------------------------------------

#' @rdname igfet_var2zscore
#' @export
igfet_hccm2centile <- function(gagedays, hccm) {
  igfet_value2centile(gagedays, hccm, var = "hc")
}

#' @rdname igfet_var2zscore
#' @export
igfet_bpdcm2centile <- function(gagedays, bpdcm) {
  igfet_value2centile(gagedays, bpdcm, var = "bpd")
}

#' @rdname igfet_var2zscore
#' @export
igfet_ofdcm2centile <- function(gagedays, ofdcm) {
  igfet_value2centile(gagedays, ofdcm, var = "ofd")
}

#' @rdname igfet_var2zscore
#' @export
igfet_accm2centile <- function(gagedays, accm) {
  igfet_value2centile(gagedays, accm, var = "ac")
}

#' @rdname igfet_var2zscore
#' @export
igfet_flcm2centile <- function(gagedays, flcm) {
  igfet_value2centile(gagedays, flcm, var = "fl")
}

## zscore2**
##---------------------------------------------------------

#' Convert INTERGROWTH z-scores/centiles to fetal ultrasound measurements
#'
#' @param gagedays gestational age in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @references International standards for fetal growth based on serial ultrasound measurements: the Fetal Growth Longitudinal Study of the INTERGROWTH-21st Project
#' Papageorghiou, Aris T et al.
#' The Lancet, Volume 384, Issue 9946, 869-879
#' @examples
#' # get value for median head circumference for child at 100 gestational days
#' igfet_centile2hccm(100, 50)
#' @rdname igfet_zscore2var
#' @export
igfet_zscore2hccm <- function(gagedays, z = 0) {
  igfet_zscore2value(gagedays, z, var = "hc")
}

#' @rdname igfet_zscore2var
#' @export
igfet_zscore2bpdcm <- function(gagedays, z = 0) {
  igfet_zscore2value(gagedays, z, var = "bpd")
}

#' @rdname igfet_zscore2var
#' @export
igfet_zscore2ofdcm <- function(gagedays, z = 0) {
  igfet_zscore2value(gagedays, z, var = "ofd")
}

#' @rdname igfet_zscore2var
#' @export
igfet_zscore2accm <- function(gagedays, z = 0) {
  igfet_zscore2value(gagedays, z, var = "ac")
}

#' @rdname igfet_zscore2var
#' @export
igfet_zscore2flcm <- function(gagedays, z = 0) {
  igfet_zscore2value(gagedays, z, var = "fl")
}

## centile2**
##---------------------------------------------------------

#' @rdname igfet_zscore2var
#' @export
igfet_centile2hccm <- function(gagedays, p = 50) {
  igfet_centile2value(gagedays, p, var = "hc")
}

#' @rdname igfet_zscore2var
#' @export
igfet_centile2bpdcm <- function(gagedays, p = 50) {
  igfet_centile2value(gagedays, p, var = "bpd")
}

#' @rdname igfet_zscore2var
#' @export
igfet_centile2ofdcm <- function(gagedays, p = 50) {
  igfet_centile2value(gagedays, p, var = "ofd")
}

#' @rdname igfet_zscore2var
#' @export
igfet_centile2accm <- function(gagedays, p = 50) {
  igfet_centile2value(gagedays, p, var = "ac")
}

#' @rdname igfet_zscore2var
#' @export
igfet_centile2flcm <- function(gagedays, p = 50) {
  igfet_centile2value(gagedays, p, var = "fl")
}


get_igfet_pars <- function(var, ga) {
  switch(var,
    hccm = {
      mn <- -28.2849 + 1.69267 * ga ^ 2 - 0.397485 * ga ^ 2 * log(ga)
      sd <- 1.98735 + 0.0136772 * ga ^ 3 - 0.00726264 * ga ^ 3 * log(ga) +
        0.000976253 * ga ^ 3 * (log(ga)) ^ 2
    },
    bpdcm = {
      mn <- 5.60878 + 0.158369 * ga ^ 2 - 0.00256379 * ga ^ 3
      sd <- exp(0.101252 + 0.00150557 * ga ^ 3 -
        0.000771535 * ga ^ 3 * log(ga) + 0.0000999638 * ga ^ 3 * (log(ga)) ^ 2)
    },
    ofdcm = {
      mn <- -12.4097 + 0.626342 * ga ^ 2 - 0.148075 * ga ^ 2 * log(ga)
      sd <- exp(- 0.880034 + 0.0631165 * ga ^ 2 - 0.0317136 * ga ^ 2 * log(ga) +
        0.00408302 * ga ^ 2 * (log(ga)) ^ 2)
    },
    accm = {
      mn <- -81.3243 + 11.6772 * ga - 0.000561865 * ga ^ 3
      sd <- -4.36302 + 0.121445 * ga ^ 2 - 0.0130256 * ga ^ 3 +
        0.00282143 * ga ^ 3 * log(ga)
    },
    flcm = {
      mn <- -39.9616 + 4.32298 * ga - 0.0380156 * ga ^ 2
      sd <- exp(0.605843 - 42.0014 * ga ^ (-2) + 0.00000917972 * ga ^ 3)
    }
  )
  # we are using centimeters
  return(list(mean = mn / 10, sd = sd / 10))
}
