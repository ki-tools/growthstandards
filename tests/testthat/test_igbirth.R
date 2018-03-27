# nolint start

# igb_wtkg2zscore(33 * 7, 1.86)
# igb_wtkg2zscore(33 * 7 + 1, 1.90)
# gamlss.dist::pST3(
#   1.86,
#   1.795602, # mu
#   0.348924, # sigma
#   1.126787, # nu
#   19.65738  # tau
# )

context("igbirth")

chkpts <- read.csv(textConnection("var,sex,ga,p3,p5,p10,p50,p90,p95,p97
lencm,Male,232,39.89,40.45,41.28,43.98,46.70,47.54,48.11
lencm,Male,300,48.49,48.89,49.49,51.44,53.40,54.01,54.42
lencm,Female,232,39.98,40.46,41.19,43.57,45.87,46.55,47.01
lencm,Female,300,47.59,48.00,48.60,50.58,52.50,53.07,53.45
wtkg,Male,232,1.22,1.32,1.47,1.99,2.56,2.74,2.86
wtkg,Male,300,2.96,3.06,3.21,3.71,4.25,4.43,4.54
wtkg,Female,231,1.20,1.29,1.41,1.86,2.35,2.51,2.61
wtkg,Female,232,1.24,1.33,1.45,1.90,2.40,2.55,2.66
wtkg,Female,300,2.80,2.90,3.04,3.53,4.08,4.26,4.37
hcircm,Male,232,28.35,28.69,29.21,30.97,32.78,33.33,33.69
hcircm,Male,300,33.33,33.60,34.00,35.38,36.80,37.23,37.51
hcircm,Female,232,28.03,28.36,28.86,30.55,32.32,32.86,33.22
hcircm,Female,300,32.67,32.93,33.31,34.60,35.95,36.36,36.63"),
stringsAsFactors = FALSE)

test_that("vectorized intergrowth birth standard to centile works", {
  a <- igb_centile2value(chkpts$ga,
    var = chkpts$var, sex = chkpts$sex)
  expect_true(all(abs(a - chkpts$p50) < 0.015))

  a <- igb_zscore2value(chkpts$ga,
    var = chkpts$var, sex = chkpts$sex)
  expect_true(all(abs(a - chkpts$p50) < 0.015))
})

test_that("vectorized intergrowth birth standard centile to value works", {
  a <- igb_value2centile(chkpts$ga, chkpts$p50,
    var = chkpts$var, sex = chkpts$sex) / 100
  expect_true(all(abs(a - 0.5) < 0.015))

  a <- igb_value2zscore(chkpts$ga, chkpts$p50,
    var = chkpts$var, sex = chkpts$sex)
  expect_true(all(abs(a - 0) < 0.03))
})

# test the very early preterm standard as well
echkpts <- read.csv(textConnection("zz_m2,zz_p2,s,m,ga
27.02,37.41,Male,lencm,168
28.29,38.68,Male,lencm,175
29.56,39.95,Male,lencm,182
30.83,41.22,Male,lencm,189
32.1,42.49,Male,lencm,196
33.37,43.76,Male,lencm,203
34.64,45.03,Male,lencm,210
35.91,46.3,Male,lencm,217
37.18,47.57,Male,lencm,224
26.59,36.99,Female,lencm,168
27.86,38.26,Female,lencm,175
29.13,39.53,Female,lencm,182
30.4,40.8,Female,lencm,189
31.67,42.07,Female,lencm,196
32.94,43.34,Female,lencm,203
34.21,44.61,Female,lencm,210
35.48,45.88,Female,lencm,217
36.75,47.15,Female,lencm,224
0.43,0.94,Male,wtkg,168
0.5,1.07,Male,wtkg,175
0.56,1.22,Male,wtkg,182
0.64,1.39,Male,wtkg,189
0.73,1.58,Male,wtkg,196
0.83,1.79,Male,wtkg,203
0.93,2.02,Male,wtkg,210
1.05,2.28,Male,wtkg,217
1.18,2.56,Male,wtkg,224
0.41,0.89,Female,wtkg,168
0.47,1.01,Female,wtkg,175
0.53,1.16,Female,wtkg,182
0.61,1.31,Female,wtkg,189
0.69,1.49,Female,wtkg,196
0.78,1.69,Female,wtkg,203
0.88,1.91,Female,wtkg,210
0.99,2.15,Female,wtkg,217
1.12,2.42,Female,wtkg,224
19.22,25.46,Male,hcircm,168
20.11,26.35,Male,hcircm,175
21,27.24,Male,hcircm,182
21.88,28.12,Male,hcircm,189
22.77,29.01,Male,hcircm,196
23.66,29.9,Male,hcircm,203
24.55,30.79,Male,hcircm,210
25.43,31.67,Male,hcircm,217
26.32,32.56,Male,hcircm,224
18.97,25.21,Female,hcircm,168
19.86,26.1,Female,hcircm,175
20.75,26.99,Female,hcircm,182
21.63,27.87,Female,hcircm,189
22.52,28.76,Female,hcircm,196
23.41,29.65,Female,hcircm,203
24.3,30.54,Female,hcircm,210
25.18,31.42,Female,hcircm,217
26.07,32.31,Female,hcircm,224
"),
stringsAsFactors = FALSE)

test_that("vectorized intergrowth early birth z-score to value works", {
  a <- igb_zscore2value(echkpts$ga, 2,
    var = echkpts$m, sex = echkpts$s)
  expect_true(all(abs(a - echkpts$zz_p2) < 0.015))

  a <- igb_zscore2value(echkpts$ga, -2,
    var = echkpts$m, sex = echkpts$s)
  expect_true(all(abs(a - echkpts$zz_m2) < 0.015))
})

test_that("vectorized intergrowth early birth value to z-score works", {
  a <- igb_value2zscore(echkpts$ga, echkpts$zz_m2,
    var = echkpts$m, sex = echkpts$s)
  expect_true(all(abs(a + 2) < 0.05))

  a <- igb_value2zscore(echkpts$ga, echkpts$zz_p2,
    var = echkpts$m, sex = echkpts$s)
  expect_true(all(abs(a - 2) < 0.05))
})

# nolint end
