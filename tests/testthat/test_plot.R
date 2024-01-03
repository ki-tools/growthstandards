# nolint start

context("plots")

# add haz to cpp through functions as it is missing by default now.

cpp$haz <- who_htcm2zscore(cpp$agedays, cpp$htcm, sex = cpp$sex)

test_that("plots work", {
  cpp$wtkg50 <- who_centile2value(cpp$agedays, y_var = "wtkg")

  # look at Male birth lengths superposed on INTERGROWTH birth standard
  # first we need just 1 record per subject with subject-level data
  cppsubj <- subset(cpp, !duplicated(cpp$subjid))
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
