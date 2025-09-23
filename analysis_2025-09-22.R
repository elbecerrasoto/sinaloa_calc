library(tidyverse)

INPUT <- "data/output.tsv"

results <- read_tsv(INPUT)


get_percent <- function(x) {
  x / sum(x)
}

DIR_INDIR <- c(
  "directos", "indirectos",
  "desbordamiento", "retroalimentacion"
)


percents_effects <- results |>
  select(all_of(DIR_INDIR)) |>
  as.matrix() |>
  apply(1, get_percent) |>
  t() |>
  as_tibble() |>
  set_names(str_c("P_", DIR_INDIR))

EFFECTS <- c(
  "directos", "indirectos",
  "desbordamiento", "retroalimentacion"
)

get_effects <- function(x) {
  percents_effects |>
    map(\(effect) effect * x) |>
    set_names(EFFECTS)
}


shocks_names <- results |>
  select(starts_with("shock")) |>
  names()

OUT <- vector(mode = "list", length = length(shocks_names))
OUT <- set_names(OUT, shocks_names)
for (sname in shocks_names) {
  resin <- results[[sname]]
  resout <- map(get_effects(resin), sum)
  OUT[[sname]] <- unlist(resout)
}

effects_over_shocks <-
  as_tibble(OUT) |>
  mutate(effect = EFFECTS) |>
  relocate(effect)

effects_over_shocks |>
  write_tsv("data/effects_on_shocks.tsv")
