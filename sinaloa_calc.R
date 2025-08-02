library(tidyverse)
source("helper.R")

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/enoe_sinaloa.tsv"

mip <- read_tsv(MIP)
employment <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))

sinaloa <- get_ZAB_LG_fx_Madds(mip)

get_T <- function(L, x, E) {
  e <- E / x
  Tm <- diag(e) %*% L
  mask <- is.nan(Tm) | is.infinite(Tm) | is.na(Tm)
  Tm[mask] <- 0
  Tm
}

etype <- c("empleos", "formales", "informales")

Tsin <- employment[etype] |>
  map(get_T, L = sinaloa$L, x = sinaloa$x)

Tsin <- set_names(Tsin, etype)
