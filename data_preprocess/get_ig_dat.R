bl <- read.csv("data_preprocess/data/intergrowth/INTERGROWTH_newborn_standards_parameters_BL.csv",
  stringsAsFactors = FALSE)
bw <- read.csv("data_preprocess/data/intergrowth/INTERGROWTH_newborn_standards_parameters_BW.csv",
  stringsAsFactors = FALSE)
hc <- read.csv("data_preprocess/data/intergrowth/INTERGROWTH_newborn_standards_parameters_HC.csv",
  stringsAsFactors = FALSE)

bls <- lapply(split(bl, bl$sex), function(x) {
  x[, c("ga", "mu", "sigma", "nu", "tau")]
})

bws <- lapply(split(bw, bw$sex), function(x) {
  x[, c("ga", "mu", "sigma", "nu", "tau")]
})

hcs <- lapply(split(hc, hc$sex), function(x) {
  x[, c("ga", "mu", "sigma", "nu", "tau")]
})

ig_coefs <- list(
  lencm = bls,
  wtkg = bws,
  hcircm = hcs
)

save(ig_coefs, file = "data/ig_coefs.rda")

# better compression
tools::checkRdaFiles("data/ig_coefs.rda")
tools::resaveRdaFiles("data/ig_coefs.rda")
