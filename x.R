library(tidyverse)

# ------ globals

INPUT <- "data/output.tsv"
EFFECTS <- c(
  "directos", "indirectos",
  "desbordamiento", "retroalimentacion"
)
# ------ helpers

get_dir_indir <- function(results) {
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

  as_tibble(OUT) |>
    mutate(effect = EFFECTS) |>
    relocate(effect)
}

# ------ main

results <- read_tsv(INPUT)

dentro <- get_dir_indir(results |> filter(region == "sinaloa"))
fuera <- get_dir_indir(results |> filter(region == "fuera_sinaloa"))

dentro <- dentro |>
  select(-effect) |>
  t() |>
  `colnames<-`(EFFECTS)

fuera <- fuera |>
  select(-effect) |>
  t() |>
  `colnames<-`(str_c(EFFECTS, "FUERA"))

fuera_vars <- rownames(fuera)
dentro_vars <- rownames(dentro)

dentro_tib <- dentro |>
  as_tibble() |>
  mutate(shock = dentro_vars) |>
  relocate(shock)

fuera_tib <- fuera |>
  as_tibble() |>
  mutate(shock = fuera_vars) |>
  relocate(shock)

Seff <- left_join(dentro_tib, fuera_tib, join_by(shock))
Seff_rowsum <- Seff |>
  select(-shock) |>
  rowSums()

Seff <- Seff |>
  mutate(total = Seff_rowsum) |>
  relocate(total, .after = shock)

write_tsv(Seff, "SHeffs.tsv")
