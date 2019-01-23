# nolint start

context("who")

# check against who docs
# e.g. http://www.who.int/childgrowth/standards/LFA_girls_0_13_percentiles.pdf?ua=1

# check first 4 weeks, 32 months, 48 months
check_seq <- c(seq(0, 28, by = 7), 974, 1461)

test_that("who quantile calculations are correct", {
  # females
  expect_true(all(round(who_centile2value(check_seq, 50), 1) ==
    c(49.1, 50.3, 51.5, 52.5, 53.4, 92.2, 102.7)))
  expect_true(all(round(who_centile2value(check_seq, 1), 1) ==
    c(44.8, 45.9, 47.1, 48.0, 48.9, 83.8, 92.7)))
  expect_true(all(round(who_centile2value(check_seq, 99), 1) ==
    c(53.5, 54.7, 55.9, 56.9, 57.9, 100.6, 112.8)))

  # males
  expect_true(all(round(who_centile2value(check_seq, 50, sex = "Male"), 1) ==
    c(49.9, 51.1, 52.3, 53.4, 54.4, 93.4, 103.3)))
  expect_true(all(round(who_centile2value(check_seq, 1, sex = "Male"), 1) ==
    c(45.5, 46.7, 47.9, 48.9, 49.9, 85.2, 93.6)))
  expect_true(all(round(who_centile2value(check_seq, 99, sex = "Male"), 1) ==
    c(54.3, 55.5, 56.8, 57.9, 58.9, 101.5, 113.1)))
})

test_that("who z-score calculations are correct", {
  # z scores should work just the same
  # (since they can be converted to quantiles and we've tested quantiles)
  expect_true(all(round(who_zscore2value(check_seq, -3), 1)[1:5] ==
    c(43.6, 44.7, 45.8, 46.7, 47.5)))
  
  ## check for whz calculations
  ## x are lengths
  
  # https://www.who.int/childgrowth/standards/WFL_girls_0_2_zscores.pdf?ua=1
  expect_true(all(round(who_zscore2value(c(65, 85, 99.5, 110), -3, x_var = "lencm", y_var = "wtkg"),1) ==
  c(5.5, 8.7, 11.5, 14)))
  
  # https://www.who.int/childgrowth/standards/WFH_girls_2_5_zscores.pdf?ua=1
  expect_true(all(round(who_zscore2value(c(65, 85, 99.5, 110), -3, x_var = "htcm", y_var = "wtkg"), 1) ==
  c(5.6, 8.8, 11.6, 14.2)))
  
  
})

test_that("who z-scores invert correctly", {
  # check getting z scores and converting back
  tmp <- who_zscore2value(check_seq, -3)
  expect_true(all.equal(who_value2zscore(check_seq, tmp), rep(-3, 7)))
  
  tmp_lengths <- c(59, 61.5, 64, 84, 94)
  tmp_heights <- c(84, 94, 112, 120)
  # 0-2 years
  tmp_whz <- who_zscore2value(tmp_lengths, -3, x_var = "lencm", y_var = "wtkg")
  who_value2zscore(tmp_lengths, tmp_whz, x_var = "lencm", y_var = "wtkg")
  # 2-5 years
  tmp_whz <- who_zscore2value(tmp_heights, -3, x_var = "htcm", y_var = "wtkg")
  who_value2zscore(tmp_heights, tmp_whz, x_var = "htcm", y_var = "wtkg")
  
  
})

test_that("who centiles invert correctly", {
  # check getting quantiles and converting back
  tmp <- who_centile2value(check_seq, 95)
  expect_true(all.equal(who_value2centile(check_seq, tmp), rep(95, 7)))
})

test_that("who centiles work with singleton not in data", {
  # check getting quantiles and converting back
  who_centile2value(100.7, 95)
  who_zscore2value(100.7, 2)
})

test_that("who calculations with incorrect y_var", {
  expect_error(who_centile2value(100.7, 95, y_var = "asdf"))
})

test_that("who calculations work with data argument", {
  # 99th percentile of weight vs. age for males from age 0 to 1461 days
  dat <- data.frame(x = rep(seq(0, 1461, length = 100), 2),
    sex = rep(c("Male", "Female"), each = 100))

  expect_error(who_centile2value(x, p = 99, y_var = "wtkg",
    sex = sx, data = dat))

  who_centile2value(x, p = 99, y_var = "wtkg", sex = sex, data = dat)
})

# nolint end
