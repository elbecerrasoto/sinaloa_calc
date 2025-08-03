library(tidyverse)
source("helper.R")

# ---- globals

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/enoe_sinaloa.tsv"
SHOCKS <- "data/shocks.tsv"

# ---- helpers

get_T <- function(L, x, E) {
  e <- E / x
  Tm <- diag(e) %*% L
  mask <- is.nan(Tm) | is.infinite(Tm) | is.na(Tm)
  Tm[mask] <- 0
  Tm
}

# ---- main

sinaloa <- read_tsv(MIP) |>
  get_ZAB_LG_fx_Madds()

employment <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))

shocks <- read_tsv(SHOCKS)

# Calculate employment matrices
etype <- c("empleos", "formales", "informales")

Tsin <- employment[etype] |>
  map(get_T, L = sinaloa$L, x = sinaloa$x)

Tsin <- set_names(Tsin, etype)
