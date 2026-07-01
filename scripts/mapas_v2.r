criar_mapas_quintis <- function(
  diretorio = getwd()
){

  #------------------------------------------------------------------
  # Pacotes
  #------------------------------------------------------------------
  library(dplyr)
  library(sf)
  library(mapview)
  library(RColorBrewer)

  #------------------------------------------------------------------
  # Carregar arquivos
  #------------------------------------------------------------------
  old <- getwd()
  on.exit(setwd(old))

  setwd(diretorio)

  load("buffers_1km.RData")
  load("parques_sf.RData")
  load("renda_df.RData")
  load("setores_sf.RData")

  #------------------------------------------------------------------
  # Construção dos quintis populacionais
  #------------------------------------------------------------------

  renda_df_quintis <- renda_df %>%
    filter(
      !is.na(populacao),
      !is.na(renda_responsavel)
    ) %>%
    group_by(code_muni) %>%
    arrange(renda_responsavel, .by_group = TRUE) %>%
    mutate(
      pop_acum = cumsum(populacao),
      prop_acum = pop_acum / sum(populacao),
      quintil_pop = case_when(
        prop_acum <= 0.20 ~ "Q1",
        prop_acum <= 0.40 ~ "Q2",
        prop_acum <= 0.60 ~ "Q3",
        prop_acum <= 0.80 ~ "Q4",
        TRUE              ~ "Q5"
      )
    ) %>%
    ungroup() %>%
    st_drop_geometry() %>%
    select(code_tract, quintil_pop)

  #------------------------------------------------------------------
  # Adicionar quintis aos setores
  #------------------------------------------------------------------

  setores_sf <- setores_sf %>%
    left_join(
      renda_df_quintis,
      by = "code_tract"
    ) %>%
    mutate(
      quintil_pop = factor(
        quintil_pop,
        levels = c("Q1","Q2","Q3","Q4","Q5")
      )
    )

  #------------------------------------------------------------------
  # Municípios
  #------------------------------------------------------------------

  codigos <- sort(unique(parques_sf$cod_mun))

  #------------------------------------------------------------------
  # Criar mapas
  #------------------------------------------------------------------
####ajuste para legendas
parques_sf <- parques_sf %>%
  mutate(tipo = "Parques")

  buffers_1km <- buffers_1km %>%
  mutate(tipo = "Buffer 1 km")

  ###criar mapas

  mapas <- lapply(codigos, function(cod){

    setores_cidade <- setores_sf %>%
      filter(code_muni.x == cod)

    buffers_cidade <- buffers_1km %>%
      filter(cod_mun == cod)

    parques_cidade <- parques_sf %>%
      filter(cod_mun == cod)

    ## Setores por quintil
    mapa_setores <- mapview(
      setores_cidade,
      zcol = "quintil_pop",
      layer.name = "Quintil de renda",
      alpha.regions = 0.50,
      na.color = "#D9D9D9",
      col.regions = brewer.pal(5, "RdYlGn")
    )

    ## Buffer

    mapa_buffers <- mapview(
  buffers_cidade,
  zcol = "tipo",
  color = "black",
  alpha.regions = 0,
  lwd = 3,
  layer.name = "Buffer 1 km",
  legend = FALSE
)

    ## Parques (todos iguais)
    mapa_parques <- mapview(
  parques_cidade,
  zcol = "tipo",
  col.regions = "black",
  cex = 4,
  layer.name = "Parques",
  legend = TRUE
)
   

    mapa_buffers +
    mapa_setores +
      
      mapa_parques

  })

  # Tabela código -> nome do município
nomes_municipios <- parques_sf %>%
  st_drop_geometry() %>%
  distinct(cod_mun, municipio) %>%   # ou "municipio", dependendo do nome da coluna
  arrange(cod_mun)

# Nomear a lista pelos nomes das cidades
names(mapas) <- nomes_municipios$municipio[
  match(codigos, nomes_municipios$cod_mun)
]

  return(mapas)

}


mapas <- criar_mapas_quintis(
  diretorio = "/Users/germano/Library/CloudStorage/OneDrive-Pessoal/XUSP/Producoes/Resumos/resumo simposio de atividade fisica e saude regiao sudeste 2026"
)











