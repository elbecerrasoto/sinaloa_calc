library(tidyverse)
source("helper.R")

# ---- globals

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/enoe_sinaloa.tsv"
SHOCKS <- "data/shocks.tsv"

# ---- helpers

# ---- main

sinaloa <- read_tsv(MIP) |>
  get_ZAB_LG_fx_Madds()

employment <- read_tsv(EMPLOYMENT) |>
  filter(!is.na(sector))

shocks <- read_tsv(SHOCKS,
  col_types = cols(scian = "c")
)

# Calculate employment matrices
etype <- c("empleos", "formales", "informales")

Tsin <- employment[etype] |>
  map(get_T, L = sinaloa$L, x = sinaloa$x)

Tsin <- set_names(Tsin, etype)


shocks70 <- left_join(employment, shocks, join_by(region, scian, sector)) |>
  select(region, scian, sector, starts_with("SH"))

shocks70 <- shocks70 |>
  mutate(across(
    starts_with("SH"),
    \(x) coalesce(x, 0)
  ))

shocks70 <- shocks70 |>
  mutate(SHtotal = rowSums(across(starts_with("SH"))))

# ----exploration

attach(sinaloa)
attach(shocks70)

sum(L %*% (SHconsA1 + f)) - sum(L %*% (f))

Tsin$empleos %*% shocks70$SHconsA1
