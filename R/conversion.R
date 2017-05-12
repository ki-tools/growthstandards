#' Unit conversion utility functions
#'
#' @param x value(s) to convert
#' @export
#' @rdname unit_conversion
cm2in <- function(x) x / 2.54

#' @export
#' @rdname unit_conversion
in2cm <- function(x) x * 2.54

#' @export
#' @rdname unit_conversion
lb2kg <- function(x) x / 2.20462262

#' @export
#' @rdname unit_conversion
kg2lb <- function(x) x * 2.20462262

#' @export
#' @rdname unit_conversion
days2years <- function(x) x / 365.25

#' @export
#' @rdname unit_conversion
years2days <- function(x) x * 365.25

#' @export
#' @rdname unit_conversion
days2months <- function(x) x / 30.4375

#' @export
#' @rdname unit_conversion
months2days <- function(x) x * 30.4375

#' @export
#' @rdname unit_conversion
months2years <- function(x) x / 12

#' @export
#' @rdname unit_conversion
years2months <- function(x) x * 12
