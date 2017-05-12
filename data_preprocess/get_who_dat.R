# sources:
# http://www.who.int/childgrowth/software/en/
# http://www.who.int/growthref/tools/en/

# wget http://www.who.int/entity/childgrowth/software/igrowup_R.zip
# wget http://www.who.int/entity/growthref/tools/who2007_R.zip

weianthro <- read.table("data_preprocess/data/igrowup_R/weianthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
lenanthro <- read.table("data_preprocess/data/igrowup_R/lenanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
bmianthro <- read.table("data_preprocess/data/igrowup_R/bmianthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
hcanthro  <- read.table("data_preprocess/data/igrowup_R/hcanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
acanthro  <- read.table("data_preprocess/data/igrowup_R/acanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
ssanthro  <- read.table("data_preprocess/data/igrowup_R/ssanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
tsanthro  <- read.table("data_preprocess/data/igrowup_R/tsanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
wflanthro <- read.table("data_preprocess/data/igrowup_R/wflanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
wfhanthro <- read.table("data_preprocess/data/igrowup_R/wfhanthro.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)

source("data_preprocess/data/igrowup_R/igrowup_standard.r")
source("data_preprocess/data/igrowup_R/igrowup_restricted.r")

# add in extensions
bmianthro2 <- read.table("data_preprocess/data/who2007_R/bfawho2007.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
weianthro2 <- read.table("data_preprocess/data/who2007_R/wfawho2007.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)
lenanthro2 <- read.table("data_preprocess/data/who2007_R/hfawho2007.txt",
  header = T, sep = "", skip = 0, stringsAsFactors = FALSE)

bmianthro2$age <- bmianthro2$age * (365.25 / 12)
weianthro2$age <- weianthro2$age * (365.25 / 12)
lenanthro2$age <- lenanthro2$age * (365.25 / 12)

bmianthro2$loh <- "H"
lenanthro2$loh <- "H"

bmianthro <- rbind(bmianthro, bmianthro2)
weianthro <- rbind(weianthro, weianthro2)
lenanthro <- rbind(lenanthro, lenanthro2)

# in lenanthro
# loh stands for "length or height"
# it is "L" up until 2 years, then it changes to "H"

who_dat <- list(
  wtkg_agedays   = weianthro, # weight
  htcm_agedays   = lenanthro, # height
  bmi_agedays    = bmianthro, # bmi
  hcircm_agedays = hcanthro,  # head circumference
  muaccm_agedays = acanthro,  # arm circumference
  ss_agedays     = ssanthro,  # subscapular skinfold
  tsftmm_agedays = tsanthro,  # triceps skinfold
  wtkg_lencm     = wflanthro, # weight for length
  wtkg_htcm      = wfhanthro  # weight for height
)

hdn <- names(who_dat)

xvar <- structure(as.list(rep("age", 9)), names = names(who_dat))
xvar$wt_ln <- "length"
xvar$wt_ht <- "height"

tmp <- strsplit(names(who_dat), "_")
names(tmp) <- names(who_dat)
xvar <- lapply(tmp, "[[", 1)
yvar <- lapply(tmp, "[[", 2)

who <- lapply(hdn, function(nm) {
  d <- who_dat[[nm]]
  names(d)[2] <- "x"
  res <- lapply(as.integer(1:2), function(ss) {
    structure(list(data = subset(d, sex == ss)[, setdiff(names(d), "sex")],
      xvar = xvar[[nm]], yvar = yvar[[nm]]), class = "whoCoef")
  })
  names(res) <- c("Male", "Female")
  res
})
names(who) <- hdn

who_coefs <- who

save(who_coefs, file = "data/who_coefs.rda")

# better compression
tools::checkRdaFiles("data/who_coefs.rda")
tools::resaveRdaFiles("data/who_coefs.rda")
