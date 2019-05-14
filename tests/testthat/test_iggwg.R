# nolint start

context("iggwg")

checkpoints <- read.csv(textConnection("ga, p3, p50, p97
15, -1.8, 0.4, 3.2
16, -1.3, 1.0, 4.0
17, -0.8, 1.6, 4.7
18, -0.4, 2.2, 5.5
19, 0.0, 2.75, 6.3
20, 0.4, 3.3, 7.1
21, 0.8, 3.8, 7.9
22, 1.1, 4.4, 8.7
23, 1.4, 4.9, 9.5
24, 1.8, 5.4, 10.3
25, 2.1, 5.9, 11.2
"), stringsAsFactors = FALSE)

test_that("intergrowth gestational weight gain calculations are correct", {
  # nolint start
  # https://intergrowth21.tghn.org/site_media/media/medialibrary/2017/05/GROW_GWG-nw-ct_Table.pdf
  # nolint end
  suppressMessages({
    for (i in seq_len(nrow(checkpoints))) {
      cp <- checkpoints[i, ]
      expect_true(abs(iggwg_value2centile(
        cp$ga * 7, cp$p3) / 100 - 0.03) < 0.01)
      expect_true(abs(iggwg_value2centile(
        cp$ga * 7, cp$p50) / 100 - 0.5) < 0.01)
      expect_true(abs(iggwg_value2centile(
        cp$ga * 7, cp$p97) / 100 - 0.97) < 0.01)
    }
  })
})

test_that("vectorized iggwg transformations work", {
  cp2 <- checkpoints
  expect_true(all(abs(iggwg_value2centile(
    cp2$ga * 7, cp2$p3) / 100 - 0.03) < 0.01))
})

test_that("intergrowth fetal calculations invert correctly", {
  a <- iggwg_value2centile(14 * 7, 0.9)
  expect_equal(0.9, iggwg_centile2value(14 * 7, a))

  a <- iggwg_value2centile(14 * 7, 0)
  expect_equal(0, iggwg_centile2value(14 * 7, a))

  a <- iggwg_value2centile(14 * 7, -1)
  expect_equal(-1, iggwg_centile2value(14 * 7, a))
})

# nolint end
