library(readxl)
library(dplyr)
library(tidyr)
library(geobr)
library(censobr)
library(sf)
library(purrr)
library(mapview)
library(RColorBrewer)
library(ggplot2)
setwd("")
rm(list=ls())

#abrir arquivos RData



load("buffers_1km.RData")
load("lista_parques_com_renda.RData")
load("parques_sf.RData")
load("renda_df.RData")
load("resultado_renda_parques.RData")
load("setores_sf.RData")

### mapas

setores_sf <- setores_sf %>%
  mutate(
    faixa_renda = case_when(
      renda_responsavel > 20 * 1621 ~ "Acima de 20 SM",
      renda_responsavel >= 10 * 1621 ~ "10 a 20 SM",
      renda_responsavel >= 4 * 1621 ~ "4 a 10 SM",
      renda_responsavel > 2 * 1621 ~ "2 a 4 SM",
      renda_responsavel <= 2 * 1621 ~ "Até 2 SM",
      TRUE ~ NA_character_
    ),
    
    faixa_renda = factor(
      faixa_renda,
      levels = c(
        "Até 2 SM",
        "2 a 4 SM",
        "4 a 10 SM",
        "10 a 20 SM",
        "Acima de 20 SM"
      )
    )
  )


codigos_municipios <- sort(unique(parques_sf$cod_mun))




codigos <- sort(unique(parques_sf$cod_mun))

mapas <- lapply(codigos, function(cod){

  setores_cidade <- setores_sf %>%
    filter(code_muni.x == cod)

  parques_cidade <- parques_sf %>%
    filter(cod_mun == cod)

  buffers_cidade <- buffers_1km %>%
    filter(cod_mun == cod)

  mapa_setores <- mapview(
    setores_cidade,
    zcol = "faixa_renda",
    layer.name = "Renda",
    alpha.regions = 0.5,
    na.color = "#D9D9D9",
    col.regions = brewer.pal(5, "YlGnBu")
  )

  mapa_buffers <- mapview(
    buffers_cidade,
    color = "red",
    alpha.regions = 0.05,
    lwd = 2,
    layer.name = "Buffer 1 km"
  )

  mapa_parques <- mapview(
    parques_cidade,
    zcol = "nome_parque",
    layer.name = "Parques",
    cex = 8
  )

  mapa_setores + mapa_buffers + mapa_parques
})

names(mapas) <- codigos