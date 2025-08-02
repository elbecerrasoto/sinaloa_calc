library(tidyverse)
source("helper.R")

MIP <- "data/mip_sinaloa.tsv"
EMPLOYMENT <- "data/enoe_sinaloa.tsv"

mip <- read_tsv(MIP)
employment <- read_tsv(EMPLOYMENT)

sinaloa <- get_ZAB_LG_fx_Madds(mip)

attach(sinaloa)
