#' Utility functions for adding growth standard bands to ggplot2/lattice plots
#'
#' @param x,x_seq value or vector of values that correspond to a measure defined by \code{x_var}. \code{x_seq} is used with geom_*
#' @param x_var x variable name (typically "agedays")
#' @param y_var y variable name (typically "htcm" or "wtkg")
#' @param var variable name for y axis for igb or igfet plots ("lencm", "wtkg", or "hcircm" for igb; "accm", "bpdcm", "flcm", "hccm", or "ofdcm" for igfet)
#' @param gagebrth gestational age at birth in days (for igb plots)
#' @param gagedays gestational age in days (for igfet plots)
#' @param sex "Male" or "Female"
#' @param p centiles at which to draw the growth standard band polygons (only need to specify on one side of the median)
#' @param color,shade optional color to use for bands (will use \code{sex} to determine if not specified). \code{shade} is used with geom_*
#' @param alpha transparency of the bands
#' @param center should the bands be centered around the median?
#' @param labels should the centiles be labeled? (not implemented)
#' @param x_trans transformation function to be applied to x-axis
#' @param y_trans transformation function to be applied to y-axis
# @param x_units units of age x-axis (days, months, or years)
#' @param data,mapping,inherit.aes supplied direclty to \code{ggplot2::layer}
#' @param standard standard name to use.  Either \code{"who"}, \code{"igb"}, or \code{"igfet"}
#' @importFrom lattice panel.polygon panel.lines
#' @examples
#' \dontrun{
#' #### ggplot2
#'
#' library(ggplot2)
#' p <- ggplot(data = subset(cpp, subjid == 8), aes(x = agedays, y = htcm)) +
#'   geom_who(x_seq = seq(0, 2600, by = 10), y_var = "htcm") +
#'   geom_point()
#'
#' #### lattice
#'
#' library(lattice)
#' xyplot(wtkg ~ agedays, data = subset(cpp, subjid == 8),
#'   panel = function(x, y, ...) {
#'     panel.who(x = seq(0, 2558, by = 30),
#'       sex = "Male", y_var = "wtkg", p = 100 * pnorm(-3:0))
#'     panel.xyplot(x, y, ...)
#'   },
#'   col = "black"
#' )
#'
#' # look at Male birth lengths superposed on INTERGROWTH birth standard
#' # first we need just 1 record per subject with subject-level data
#' cppsubj <- subset(cpp, !duplicated(cpp$subjid))
#' xyplot(birthlen ~ jitter(gagebrth), data = subset(cppsubj, sex == "Male"),
#'   panel = function(x, y, ...) {
#'     panel.igb(gagebrth = 250:310, var = "lencm", sex = "Male")
#'     panel.points(x, y, ...)
#'   },
#'   col = "black", alpha = 0.75,
#'   xlab = "Gestational Age at Birth (days)", ylab = "Birth Length (cm)"
#' )
#' }
#' @rdname plot_growth
#' @export
panel.who <- function(
  x,
  x_var = "agedays", y_var = "htcm",
  sex = "Female",
  p = c(1, 5, 25, 50),
  color = NULL, alpha = 0.15,
  center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  panel_growthstandard(x = x, x_var = x_var, y_var = y_var, sex = sex,
    p = p, color = color, alpha = alpha, center = center, labels = labels,
    x_trans = x_trans, y_trans = y_trans, standard = "who")
}

#' @rdname plot_growth
#' @export
panel.igb <- function(gagebrth, var = "lencm", sex = "Female",
  p = c(1, 5, 25, 50), color = NULL, alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  panel_growthstandard(x = gagebrth, x_var = "gagebrth", y_var = var,
    sex = sex, p = p, color = color, alpha = alpha, center = center,
    labels = labels, x_trans = x_trans, y_trans = y_trans, standard = "igb")
}

#' @rdname plot_growth
#' @export
panel.igfet <- function(gagedays, var = "hccm",
  p = c(1, 5, 25, 50), color = "green", alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  panel_growthstandard(x = gagedays, x_var = "gagedays", y_var = var,
    sex = "Female", p = p, color = color, alpha = alpha, center = center,
    labels = labels, x_trans = x_trans, y_trans = y_trans, standard = "igfet")
}

panel_growthstandard <- function(x, x_var = "agedays", y_var = "htcm", sex = "Female",
  p = c(1, 5, 25, 50), color = NULL, alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity, standard = "who") {

  if (is.null(color))
    color <- ifelse(sex == "Male", "blue", "red")

  dat <- get_growth_band_data(x = x, x_var = x_var, y_var = y_var, sex = sex, p = p,
    center = center, x_trans = x_trans, y_trans = y_trans, standard = standard)

  for (dd in dat$p)
    lattice::panel.polygon(dd$x, dd$y, col = color, alpha = alpha, border = color)

  if (!is.null(dat$med))
    lattice::panel.lines(dat$med$x, dat$med$y, col = color, alpha = alpha)
}

#' @rdname plot_growth
#' @importFrom ggplot2 layer ggproto Geom draw_key_point GeomPolygon GeomPath
#' @importFrom grid gList
#' @export
geom_growthstandard <- function(
  mapping = NULL,
  data = NULL,
  x_seq,
  x_var = "agedays",
  y_var, # not set as it can't be read from the mapping
  sex = "Female",
  p = c(1, 5, 25, 50),
  shade = NULL,
  alpha = 0.15,
  center = FALSE,
  x_trans = identity,
  y_trans = identity,
  standard = "who",
  inherit.aes = TRUE
) {

  if (!is.null(mapping$x)) {
    x_var <- deparse(mapping$x)
  }
  if (!is.null(mapping$y)) {
    y_var <- deparse(mapping$y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomGrowthStandard,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      # ...,
      shade = shade,
      x_var = x_var,
      y_var = y_var,
      x_seq = x_seq,
      sex = sex,
      p = p,
      alpha = alpha,
      center = center,
      x_trans = x_trans,
      y_trans = y_trans,
      standard = standard
    )
  )
}

GeomGrowthStandard <- ggplot2::ggproto(
  "GeomGrowthStandard", ggplot2::Geom,
  required_aes = c("x", "y"),
  # default_aes = aes(color = NULL),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(
    data, panel_scales, coord,
    shade,
    x_var, y_var,
    x_seq, sex, p,
    alpha, center,
    x_trans, y_trans,
    standard
  ) {

    if (is.null(shade)) {
      color <- ifelse(identical(sex, "Male"), "blue", "red")
    } else {
      color <- shade
    }

    dat <- get_growth_band_data(
      x = x_seq, x_var = x_var, y_var = y_var, sex = sex, p = p,
      center = center, x_trans = x_trans, y_trans = y_trans, standard = standard
    )

    ret <- list()

    for (dd in dat$p) {
      poly_dt <- dd
      poly_dt$PANEL <- unique(data$PANEL)
      poly_dt$group <- 1
      poly_dt$colour <- color
      poly_dt$alpha <- alpha
      path_dt <- poly_dt

      poly_dt$linetype <- ggplot2::GeomPolygon$default_aes$linetype
      poly_dt$linewidth <- 0
      poly_dt$fill <- color

      path_dt$linetype <- ggplot2::GeomPath$default_aes$linetype
      path_dt$linewidth <- ggplot2::GeomPath$default_aes$linewidth

      ret[[length(ret) + 1]] <- ggplot2::GeomPolygon$draw_panel(data = poly_dt, panel_scales, coord)
      ret[[length(ret) + 1]] <- ggplot2::GeomPath$draw_panel(data = path_dt, panel_scales, coord)
      # obj <- obj +
      #   ggplot2::geom_polygon(data = dd, ggplot2::aes(x = x, y = y),
      #     color = color, fill = color, alpha = alpha, linewidth = 0) +
      #   ggplot2::geom_path(data = dd, ggplot2::aes(x = x, y = y),
      #     color = color, alpha = alpha)
    }

    if (!is.null(dat$med)) {
      # obj <- obj +
      #   ggplot2::geom_path(data = dat$med, ggplot2::aes(x = x, y = y),
      #     color = color, alpha = alpha)
      path_dt <- dat$med
      path_dt$PANEL <- 1
      path_dt$group <- 1
      path_dt$colour <- color
      path_dt$alpha <- alpha
      path_dt$linetype <- ggplot2::GeomPath$default_aes$linetype
      path_dt$linewidth <- ggplot2::GeomPath$default_aes$linewidth

      ret[[length(ret) + 1]] <- ggplot2::GeomPath$draw_panel(data = path_dt, panel_scales, coord)
    }

    do.call(grid::gList, ret)
  }
)

#' @rdname plot_growth
#' @param ... items supplied direclty to \code{geom_growthstandard}
#' @export
geom_who <- function(...) {
  geom_growthstandard(..., standard = "who")
}

#' @rdname plot_growth
#' @export
geom_igb <- function(..., var = "lencm") {

  geom_growthstandard(..., x_var = "gagebrth", y_var = var, standard = "igb")
}

#' @rdname plot_growth
#' @export
geom_igfet <- function(..., var = "hccm", color = "green") {

  geom_growthstandard(..., x_var = "gagedays", y_var = var, sex = "Female", standard = "igfet")
}

# geom_growthstandard_old <- function(obj, x, x_var = "agedays", y_var = "htcm",
#   sex = "Female", p = c(1, 5, 25, 50), color = NULL, alpha = 0.15, center = FALSE,
#   labels = TRUE, x_trans = identity, y_trans = identity, standard = "who") {
#
#   if (is.null(color))
#     color <- ifelse(sex == "Male", "blue", "red")
#
#   dat <- get_growth_band_data(x = x, x_var = x_var, y_var = y_var, sex = sex, p = p,
#     center = center, x_trans = x_trans, y_trans = y_trans, standard = standard)
#
#   for (dd in dat$p)
#     obj <- obj +
#       ggplot2::geom_polygon(data = dd, ggplot2::aes(x = x, y = y),
#         color = color, fill = color, alpha = alpha, linewidth = 0) +
#       ggplot2::geom_path(data = dd, ggplot2::aes(x = x, y = y),
#         color = color, alpha = alpha)
#
#   if (!is.null(dat$med))
#     obj <- obj +
#       ggplot2::geom_path(data = dat$med, ggplot2::aes(x = x, y = y),
#         color = color, alpha = alpha)
#
#   obj
# }

#' Utility functions for adding growth standard bands to ggplot2/lattice plots
#'
#' @param obj ggplot2 object to add z bands to
#' @param x range on x axis that should be covered by bands
#' @param z z-scores at which to draw bands (only need to specify on one side of zero)
#' @param color color to use for bands
#' @param alpha transparency of the bands
# @param x_units units of age x-axis (days, months, or years)
#' @rdname plot_zband
#' @export
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(data = cpp, aes(x = jitter(agedays), y = haz))
#' geom_zband(p, x = seq(0, 2600, by = 10)) +
#'   geom_point()
#'
#' library(lattice)
#' xyplot(haz ~ jitter(agedays), data = cpp,
#'   panel = function(x, y, ...) {
#'     panel.zband(x)
#'     panel.xyplot(x, y, ...)
#'   },
#'   col = "black", alpha = 0.5
#' )
#' }
panel.zband <- function(x, z = -3:0, color = "green", alpha = 0.25) {
  dat <- get_z_band_data(x = x, z = z)

  for (dd in dat$z)
    panel.polygon(dd$x, dd$y, col = color, alpha = alpha)

  if (!is.null(dat$med))
    panel.lines(dat$med$x, dat$med$y, col = color, alpha = alpha)
}

#' @rdname plot_zband
#' @export
geom_zband <- function(obj, x, z = -3:0, color = "green", alpha = 0.25) {
  dat <- get_z_band_data(x = x, z = z)

  for (dd in dat$z)
    obj <- obj +
      ggplot2::geom_polygon(data = dd, ggplot2::aes(x = x, y = y),
        color = color, fill = color, alpha = alpha, linewidth = 10) +
      ggplot2::geom_path(data = dd, ggplot2::aes(x = x, y = y),
        color = color, alpha = alpha)

  if (!is.null(dat$med))
    obj <- obj +
      ggplot2::geom_path(data = dat$med, ggplot2::aes(x = x, y = y),
        color = color, alpha = alpha)

  obj
}

# generic function to get growth standard data for lattice, ggplot
get_growth_band_data <- function(x, x_var = "agedays", y_var = "htcm",
  sex = "Female", p = c(1, 5, 25, 50), center = FALSE,
  x_trans = identity, y_trans = identity, standard = "who") {

  if (any(p > 50)) {
    warning("ignoring 'p' values that are greater than 50")
    p <- p[p <= 50]
  }

  has_median <- 50 %in% p
  if (has_median)
    p <- setdiff(p, 50)

  p <- sort(p)

  if (standard == "who") {
    centile_method <- who_centile2value
    pars <- list(x = x, x_var = x_var, y_var = y_var, sex = sex)
  } else if (standard == "igb") {
    centile_method <- igb_centile2value
    pars <- list(gagebrth = x, var = y_var, sex = sex)
  } else if (standard == "igfet") {
    centile_method <- igfet_centile2value
    pars <- list(gagedays = x, var = y_var)
  } else {
    stop("growth standard ", standard, " is not valid", call. = FALSE)
  }

  if (has_median || center)
    med <- do.call(centile_method, c(pars, list(p = 50)))

  res <- list()
  res$p <- lapply(p, function (pp) {
    val1 <- do.call(centile_method, c(pars, list(p = pp)))
    val2 <- do.call(centile_method, c(pars, list(p = 100 - pp)))
    if (center) {
      val1 <- val1 - med
      val2 <- val2 - med
    }
    tmp <- data.frame(x = x_trans(c(x, rev(x))), y = y_trans(c(val1, rev(val2))))
    tmp[complete.cases(tmp), ]
  })

  if (has_median) {
    if (center)
      med <- med - med
    tmp <- data.frame(x = x_trans(x), y = y_trans(med))
    res$med <- tmp[complete.cases(tmp), ]
  }

  res
}

get_z_band_data <- function(x, z) {
  x <- range(x, na.rm = TRUE)
  if (length(unique(x)) == 1)
    x <- x + c(-1, 1)

  if (any(z > 0)) {
    warning("ignoring 'z' values that are greater than 0")
    z <- z[z <= 0]
  }

  res <- list()
  res$z <- lapply(z, function (zz) {
    val1 <- rep(zz, 2)
    val2 <- rep(-zz, 2)
    data.frame(x = c(x, rev(x)), y = c(val1, rev(val2)))
  })

  res$med <- data.frame(x = x, y = rep(0, 2))

  res
}
