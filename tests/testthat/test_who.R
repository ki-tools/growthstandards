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
})

test_that("who z-scores invert correctly", {
  # check getting z scores and converting back
  tmp <- who_zscore2value(check_seq, -3)
  expect_true(all.equal(who_value2zscore(check_seq, tmp), rep(-3, 7)))
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
