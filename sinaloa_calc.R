library(tidyverse)

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/enoe_sinaloa.tsv"

mip <- read_tsv(MIP)
employment <- read_tsv(EMPLOYMENT)

