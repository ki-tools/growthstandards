# nolint start

context("igfet")

checkpoints <- read.csv(textConnection("var, ga, p3, p50, p97
bpd, 14,  26.34,  29.61,  32.89
bpd, 40,  87.52,  94.89, 102.26
ac,  14,  72.85,  80.61,  88.38
ac,  40, 307.66, 349.80, 391.95
fl,  14,  10.26,  13.11,  15.96
fl,  40,  66.09,  72.13,  78.17
hc,  14,  87.38,  97.88, 108.37
hc,  40, 309.64, 333.94, 358.25
ofd, 14,  30.12,  33.76,  37.40
ofd, 40, 104.12, 115.77, 127.42"), stringsAsFactors = FALSE)

test_that("intergrowth fetal quantile calculations are correct", {

  # nolint start
  # https://intergrowth21.tghn.org/site_media/media/articles/INTERGROWTH21st_Fetal_charts_Abdominal_Circumfrance_11062015.pdf
  # https://intergrowth21.tghn.org/site_media/media/articles/INTERGROWTH21st_Fetal_charts_Bi-Parietal_Diameter_11062015.pdf
  # https://intergrowth21.tghn.org/site_media/media/articles/INTERGROWTH21st_Fetal_charts_Femur_Length_11062015.pdf
  # https://intergrowth21.tghn.org/site_media/media/articles/INTERGROWTH21st_Fetal_charts_Head_Circumfrance_11062015.pdf
  # https://intergrowth21.tghn.org/site_media/media/articles/INTERGROWTH21st_Fetal_charts_Occipito_frontal_Diameter_11062015.pdf
  # nolint end

  for (i in seq_len(nrow(checkpoints))) {
    cp <- checkpoints[i, ]
    expect_true(igfet_value2centile(
      cp$ga * 7, cp$p3 / 10, var = cp$var) / 100 - 0.03 < 0.001)
    expect_true(igfet_value2centile(
      cp$ga * 7, cp$p50 / 10, var = cp$var) / 100 - 0.5 < 0.001)
    expect_true(igfet_value2centile(
      cp$ga * 7, cp$p97 / 10, var = cp$var) / 100 - 0.97 < 0.001)
  }
})

test_that("vectorized igfet transformations work", {
  cp2 <- subset(checkpoints, var == "bpd")
  expect_true(all(igfet_bpdcm2centile(
    cp2$ga * 7, cp2$p3 / 10) / 100 - 0.03 < 0.001))
})

test_that("intergrowth fetal calculations invert correctly", {
  a <- igfet_value2centile(14 * 7, 2.961, var = "bpdcm")
  expect_equal(2.961, igfet_centile2value(14 * 7, a, var = "bpdcm"))

  a <- igfet_value2centile(14 * 7, 7.285, var = "accm")
  expect_equal(7.285, igfet_centile2value(14 * 7, a, var = "accm"))

  a <- igfet_value2centile(14 * 7, 1.026, var = "flcm")
  expect_equal(1.026, igfet_centile2value(14 * 7, a, var = "flcm"))

  a <- igfet_value2centile(14 * 7, 8.738, var = "hccm")
  expect_equal(8.738, igfet_centile2value(14 * 7, a, var = "hccm"))

  a <- igfet_value2centile(14 * 7, 3.012, var = "ofdcm")
  expect_equal(3.012, igfet_centile2value(14 * 7, a, var = "ofdcm"))
})

# nolint end
