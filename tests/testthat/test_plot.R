# nolint start

context("plots")

# add haz to cpp through functions as it is missing by default now.

cpp$haz <- who_htcm2zscore(cpp$agedays, cpp$htcm, sex = cpp$sex)

test_that("plots work", {
  library(rbokeh)
  figure() %>%
    ly_who(x = seq(0, 2558, by = 30), y_var = "wtkg",
      x_trans = days2years, sex = "Male") %>%
    ly_points(days2years(agedays), wtkg,
      data = subset(cpp, subjid == 8), col = "black",
      hover = c(agedays, wtkg, lencm, htcm, bmi, geniq, sysbp, diabp))

  cpp$wtkg50 <- who_centile2value(cpp$agedays, y_var = "wtkg")
  figure() %>%
    ly_who(x = seq(0, 2558, by = 30), y_var = "wtkg", color = "blue",
      x_trans = days2years, center = TRUE) %>%
    ly_points(days2years(agedays), wtkg - wtkg50, color = "black",
      data = subset(cpp, subjid == 8))

  # look at Male birth lengths superposed on INTERGROWTH birth standard
  # first we need just 1 record per subject with subject-level data
  cppsubj <- subset(cpp, !duplicated(cpp$subjid))
  figure(
    xlab = "Gestational Age at Birth (days)",
    ylab = "Birth Length (cm)") %>%
    ly_igb(gagebrth = 250:310, var = "lencm", sex = "Male") %>%
    ly_points(jitter(gagebrth), birthlen, data = subset(cppsubj, sex == "Male"),
      color = "black")

  # plot growth standard bands at z=1, 2, 3 for fetal head circumference
  figure(xlab = "Gestational Age (days)",
    ylab = "Head Circumference (cm)") %>%
      ly_igfet(gagedays = 98:280, var = "hccm", p = pnorm(-3:0) * 100)

  #### lattice

  library(lattice)
  xyplot(wtkg ~ agedays, data = subset(cpp, subjid == 8),
    panel = function(x, y, ...) {
      panel.who(x = seq(0, 2558, by = 30),
        sex = "Male", y_var = "wtkg", p = 100 * pnorm(-3:0))
      panel.xyplot(x, y, ...)
    },
    col = "black")

  # look at Male birth lengths superposed on INTERGROWTH birth standard
  # first we need just 1 record per subject with subject-level data
  cppsubj <- subset(cpp, !duplicated(cpp$subjid))
  xyplot(birthlen ~ jitter(gagebrth), data = subset(cppsubj, sex == "Male"),
    panel = function(x, y, ...) {
      panel.igb(gagebrth = 250:310, var = "lencm", sex = "Male")
      panel.points(x, y, ...)
    },
    col = "black", alpha = 0.75,
    xlab = "Gestational Age at Birth (days)", ylab = "Birth Length (cm)"
  )

  #### ggplot2

  library(ggplot2)
  p <- ggplot(data = subset(cpp, subjid == 8), aes(x = agedays, y = htcm)) +
    geom_who(x_seq = seq(0, 2600, by = 10), y_var = "htcm") +
    geom_point()

  ## zband

  figure() %>%
    ly_zband(cpp$agedays) %>%
    ly_points(jitter(agedays), haz, data = cpp, color = "black")

  xyplot(haz ~ jitter(agedays), data = cpp,
    panel = function(x, y, ...) {
      panel.zband(x)
      panel.xyplot(x, y, ...)
    },
    col = "black", alpha = 0.5
  )

  p <- ggplot(data = cpp, aes(x = jitter(agedays), y = haz))
  geom_zband(p, x = seq(0, 2600, by = 10)) +
    geom_point()

})

# nolint end
