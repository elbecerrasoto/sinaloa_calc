# Generate Structure

# When filling input ...
# the MIP scale is on millions of pesos at 2018

SIN_GDP <- 517548.272 # Already on millions of pesos 2023

CONSTRUCTION <- 1681978e3
USD_MXN <- 18.50
MIP_SCALE <- 1e6

SH_CONS <- USD_MXN * CONSTRUCTION / MIP_SCALE

# The %6 of the GDP
RELATION_GDP <- SH_CONS / SIN_GDP


library(tidyverse)
nat <- read_tsv("data/nacional.tsv")
tot <- read_tsv("data/total.tsv")
imp <- read_tsv("data/importado.tsv")

# supply and use tables
sutall <- bind_rows(nat, tot, imp)

# write_tsv(sutall, "results/supply_use_relative.tsv")

check_industry <- function(industry) {
  tibble(
    sector = imp$sector,
    nat = nat[[industry]],
    tot = tot[[industry]],
    imp = imp[[industry]]
  ) |>
    arrange(desc(nat), desc(tot), desc(imp))
}


structs <- map(nat$sector, check_industry) |>
  set_names(nat$sector)

structs02 <- structs |>
  imap(\(x, ix) mutate(x, investor = ix)) |>
  bind_rows()


splitsQ <- read_tsv("splits.tsv")

sin_out_splitsQ <- c(splitsQ$sin, splitsQ$out) * c(
  structs$industria_quimica_plasticos$tot,
  structs$industria_quimica_plasticos$tot
)

shocksQ <- sin_out_splitsQ * SH_CONS

TemplateS <- read_tsv("data/shocks_template.tsv")

shocksReady <- TemplateS |>
  select(-shock_02) |>
  mutate(schock_01 = shocksQ)
