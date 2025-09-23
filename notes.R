# When filling input ...
# the MIP scale is on millions of pesos at 2018

SIN_GDP <- 517548.272 # Already on millions of pesos

CONSTRUCTION <- 1681978e3
USD_MXN <- 18.50
MIP_SCALE <- 1e6

SH_CONS <- USD_MXN * CONSTRUCTION / MIP_SCALE

# The %6 of the GDP
RELATION_GDP <- SH_CONS / SIN_GDP

smip <- function(usd_e3) {
  # scale mip
  USD_MXN * usd_e3 / MIP_SCALE
}


CONSTRUCTION <- c(
  "23_cons" = smip(828037.77e3),
  "333_336_maquinaria" = smip(227067.03e3 + 166515.82e3), # fuera del estado
  "46_menor" = smip(292159.58e3) # fuera del estado
)


OPERATIONS <- c(
  "54_tecnicos" = smip(8409.89e3 + 840.989e3),
  "324_quimica" = smip(5045.934e3), # fuera del estado
  "22_gas" = smip(150000e3 + 52e3),
  "46_menor" = smip(50.45934e3),
  "52_financieros" = smip(3798.5276e3) # fuera del estado
)
