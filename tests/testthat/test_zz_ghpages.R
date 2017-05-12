
context("growth standards")

expect_zscore_centile_fn <- function(
  fn_to_zscore,
  fn_to_centile,
  zscore_to_fn,
  centile_to_fn,
  time_male,
  time_female,
  name,
  standard_name
) {

  # print(str_c(standard_name, "_", type))
  expect_true(!is.null(time)) # nolint

  if (standard_name == "igfet") {
    time <- time_male
    for (random_centile in runif(5, min = 0.1, max = 0.9)) {

      maybe_random_centile <- fn_to_centile(time, centile_to_fn(time, random_centile))
      expect_equal(
        random_centile,
        unique(round(maybe_random_centile, 6)),
        tolerance = 0.00002
      )

      random_zvalue <- qnorm(random_centile)
      maybe_random_zvalue <- fn_to_zscore(time, zscore_to_fn(time, random_zvalue))
      expect_equal(
        random_zvalue,
        unique(round(maybe_random_zvalue, 6)),
        tolerance = 0.00002
      )
    }
    return()
  }


  for (sex in c("Female", "Male")) {
    time <- ifelse(sex == "Male", time_male, time_female)
    for (random_centile in runif(5, min = 0.1, max = 0.9)) {

      maybe_random_centile <- fn_to_centile(
        time,
        centile_to_fn(time, random_centile, sex = sex),
        sex = sex
      )

      expect_equal(
        random_centile,
        unique(round(maybe_random_centile, 6)),
        tolerance = 0.00002
      )

      random_zvalue <- qnorm(random_centile)

      maybe_random_zvalue <- fn_to_zscore(
        time,
        zscore_to_fn(time, random_zvalue, sex = sex),
        sex = sex
      )
      expect_equal(
        random_zvalue,
        unique(round(maybe_random_zvalue, 6)),
        tolerance = 0.00002
      )
    }
  }

}

context("Misc")

test_that("size conversion", {
  expect_equal(
    who_htcm2zscore(1670, in2cm(44)),
    1.117365,
    tolerance = 0.00002
  )

  expect_equal(
    who_wtkg2zscore(1670, lb2kg(48)),
    1.527048,
    tolerance = 0.00002
  )
})
