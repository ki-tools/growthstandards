#' @keywords internal
"_PACKAGE"

#' growthstandards: Anthropometric Growth Standard Calculations
#'
#' Convert anthropometric measurements to or from z-scores and centiles
#' using the WHO and INTERGROWTH growth standards. The WHO standard are used
#' for infants, children, and young adults. INTERGROWTH standards are
#' provided for fetal and newborn (including very preterm) calculations.
#'
#' \url{http://hbgdki.github.io/growthstandards/}
#' @name growthstandards-package
#' @aliases growthstandards
#' @examples
#' help(package = "growthstandards")
#' browseURL("http://hbgdki.github.io/growthstandards/")
#' @importFrom stats approx complete.cases pnorm qnorm
#' @import dplyr
NULL
