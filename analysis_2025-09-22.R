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


# cons <- percents_effects |>
#   map(\(effect) effect * results$shock_cons_empleos) |>
#   set_names(c("cons_directos", "cons_indirectos",
#               "cons_desbordamiento", "cons_retroalimentacion"))
# 
# ops <- percents_effects |>
#   map(\(effect) effect * results$shock_ops_year_empleos) |>
#   set_names(c("ops_directos", "ops_indirectos",
#               "ops_desbordamiento", "ops_retroalimentacion"))


get_effects <- function(x) {
  tmp <- percents_effects |>
    map(\(effect) effect * x) |>
  set_names(c("dir", "indir",
              "desb", "retro"))
  inducidos <- tmp$desb + tmp$retro
  list(directos = tmp$dir,
       indirectos = tmp$indir,
       inducidos = inducidos)
}


