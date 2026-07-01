
setwd("")
rm(list=ls())
# =========================================================
# CARREGAR OBJETOS
# =========================================================

library(sf)
library(dplyr)
library(mapview)

# =========================================================
# CARREGAR OBJETOS
# =========================================================

load("parques_sf.RData")
load("buffers_1km.RData")
load("setores_sf.RData")
load("intersec.RData")

# =========================================================
# ESCOLHER UM PARQUE DE EXEMPLO
# =========================================================

id_exemplo <- 1

parque <- parques_sf %>%
  filter(id_parque == id_exemplo)

buffer <- buffers_1km %>%
  filter(id_parque == id_exemplo)

intersec_parque <- intersec %>%
  filter(id_parque == id_exemplo)

setores <- setores_sf %>%
  filter(code_tract %in% intersec_parque$code_tract)

# =========================================================
# MAPA
# =========================================================
m1 <- mapview(
  setores,
  color = "grey50",
  col.regions = "pink",
  alpha.regions = 0.5,
  lwd = 1,
  layer.name = "Setores censitários"
)

m2 <- mapview(
  intersec_parque,
  color = "darkgreen",
  col.regions = "darkgreen",
  alpha.regions = 0.5,
  lwd = 1,
  layer.name = "Área utilizada"
)

m3 <- mapview(
  buffer,
  color = "blue",
  alpha.regions = 0,
  lwd = 3,
  layer.name = "Buffer 1 km"
)

m4 <- mapview(
  parque,
  color = "red",
  col.regions = "red",
  cex = 10,
  layer.name = "Parque"
)

m4 + m3 + m2 + m1