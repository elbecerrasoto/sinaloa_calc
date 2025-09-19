library(tidyverse)

INPUT <- "data/output.tsv"

results <- read_tsv(INPUT)


get_percent <- function(x) {
  x  / sum(x)
}

DIR_INDIR <- c("directos", "indirectos",
               "desbordamiento", "retroalimentacion")


percents_effects <- results |>
  select(all_of(DIR_INDIR)) |>
  as.matrix() |>
  apply(1, get_percent) |>
  t() |>
  as_tibble() |>
  set_names(str_c("P_", DIR_INDIR)
)

