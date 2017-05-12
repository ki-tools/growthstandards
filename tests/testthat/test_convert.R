
context("conversions")

test_that("height conversions are correct", {
  expect_equivalent(in2cm(1), 2.54)
  expect_equivalent(cm2in(2.54), 1)
})


test_that("weight conversions are correct", {
  expect_equivalent(lb2kg(1), 1 / 2.20462262)
  expect_equivalent(kg2lb(1), 2.20462262)
})


test_that("time conversions are correct", {
  expect_equivalent(days2years(365.25), 1)
  expect_equivalent(years2days(1), 365.25)
  expect_equivalent(days2months(30.4375), 1)
  expect_equivalent(months2days(1), 30.4375)
  expect_equivalent(months2years(12), 1)
  expect_equivalent(years2months(1), 12)
})
