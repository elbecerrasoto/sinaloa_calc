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

effects_pib <- shocks70 |>
  select(starts_with("SH")) |>
  map(\(sh) as.double(sinaloa$L %*% sh)) |>
  as_tibble()

Leffects_employment <- vector(mode = "list", length = 3)
for (Ttype in names(Tsin)) {
  Tm <- Tsin[[Ttype]]
  i_effects <- shocks70 |>
    select(starts_with("SH")) |>
    map(\(sh) as.double(Tm %*% sh))
  new_names <- str_c(names(i_effects), "_", Ttype)

  i_effects <- i_effects |>
    set_names(new_names) |>
    as_tibble()

  Leffects_employment[[Ttype]] <- i_effects
}
effects_employment <- bind_cols(Leffects_employment)

results <- shocks70 |>
  select(!starts_with("SH")) |>
  bind_cols(effects_pib, effects_employment)
