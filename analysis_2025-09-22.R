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

# get_effects <- function(x) {
#   tmp <- percents_effects |>
#     map(\(effect) effect * x) |>
#   set_names(c("dir", "indir",
#               "desb", "retro"))
#   inducidos <- tmp$desb + tmp$retro
#   list(directos = tmp$dir,
#        indirectos = tmp$indir,
#        inducidos = inducidos)
# }


get_effects <- function(x) {
  tmp <- percents_effects |>
    map(\(effect) effect * x) |>
  set_names(c("directos", "indirectos",
              "desbordamiento", "retroalimentacion"))
  tmp
  }


shocks_names <- results |>
  select(starts_with("shock")) |>
  names()

OUT <- vector(mode = "list", length = length(shocks_names))
OUT <- set_names(OUT, shocks_names)
for(sname in shocks_names) {
  resin <- results[[sname]]
  resout <- map(get_effects(resin), sum)
  OUT[[sname]] <- unlist(resout)
}

