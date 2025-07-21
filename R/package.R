utils::globalVariables(c("x", "y"))

#' List of WHO growth standard coefficients
#'
#' @name who_coefs
#' @docType data
#' @description
#' A list of coefficients from the WHO for various pairs of growth standards
#' that are used to compute quantiles and z-scores.  The format is a list, where
#' each element is a pairing of variables, e.g. "wtkg_agedays", "htcm_agedays",
#' "bmi_agedays", "hcircm_agedays", "muaccm_agedays", "ss_agedays",
#' "tsftmm_agedays", "wtkg_lencm""wtkg_htcm".  Within each of these elements is
#' a list for sex with names "Female" and "Male".
#' @examples
#' head(who_coefs$htcm_age$Female$data)
#' @seealso \code{\link{who_centile2value}}, \code{\link{who_value2centile}},
#' \code{\link{who_zscore2value}}, \code{\link{who_value2zscore}}
#'
#' @source
#' 0-5 years: \url{http://www.who.int/childgrowth/software/en/}
#'
#' 5-15 years: \url{http://www.who.int/growthref/tools/en/}
#' @keywords data
NULL


#' List of INTERGROWTH birth standard coefficients
#'
#' @name ig_coefs
#' @docType data
#' @description
#' A list of coefficients from the INTERGROWTH birth standard.
#' @examples
#' head(ig_coefs$hcircm$Female)
#' @seealso \code{\link{igb_centile2value}}, \code{\link{igb_value2centile}},
#' \code{\link{igb_zscore2value}}, \code{\link{igb_value2zscore}}
#' @references International standards for newborn weight, length, and head
#' circumference by gestational age and sex: the Newborn Cross-Sectional Study
#' of the INTERGROWTH-21st Project
#' Villar, José et al.
#' The Lancet, Volume 384, Issue 9946, 857-868
#' @source
#' \url{https://intergrowth21.tghn.org}
#' @keywords data
NULL

#' List of INTERGROWTH very preterm birth standard coefficients
#'
#' @name ig_early_coefs
#' @docType data
#' @description
#' A list of coefficients from the INTERGROWTH very preterm birth standard.
#' @examples
#' ig_early_coefs$hcircm$Female
#' @seealso \code{\link{igb_centile2value}}, \code{\link{igb_value2centile}},
#' \code{\link{igb_zscore2value}}, \code{\link{igb_value2zscore}}
#' @references INTERGROWTH-21st very preterm size at birth reference charts.
#' Lancet 2016 doi.org/10.1016/S0140-6736(16) 00384-6.
#' Villar, José et al.
#' @source
#' \url{https://intergrowth21.tghn.org}
#' @keywords data
NULL


#' Subset of growth data from the collaborative perinatal project (CPP)
#'
#' @name cpp
#' @docType data
#' @description
#' Subset of growth data from the collaborative perinatal project (CPP).
#' @source
#' \url{https://catalog.archives.gov/id/606622}
#'
#' Broman, Sarah. "The collaborative perinatal project: an overview." Handbook
#' of longitudinal research 1 (1984): 185-227.
#' @usage cpp
#' @keywords data
#' @examples
#' head(cpp)
NULL
