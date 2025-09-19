CONSTRUCTION <- 1681978e3
USD_MXN <- 19.249
MIP_SCALE <- 1e6

SH_CONS <- USD_MXN * CONSTRUCTION / MIP_SCALE
SH_CONS

LIFE <- 30
OPERATIONS <- c(
  "54_tecnicos" = (36350e3 + 4362e3),
  "324_326_quim" = 36350e3,
  "22_elec" = 200000e3 + 52e3,
  "46_menor" = 18175e3,
  "52_finan" = 68211e3
  )

PER_YEAR <- OPERATIONS * USD_MXN / MIP_SCALE

ALL_YEARS <- LIFE * PER_YEAR

# When filling input ...
# the scale is on millions of pesos at 2018

# check data/enoe_sinaloa.tsv
# to fill in the correct industries

# generate a template
# to fill, then fill accordingly 

