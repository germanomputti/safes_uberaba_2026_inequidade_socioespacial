library(readxl)
library(dplyr)
library(tidyr)
library(geobr)
library(censobr)
library(sf)
library(purrr)
setwd("")
rm(list=ls())



# Ler nomes das colunas
nomes_colunas <- names(read_excel("lista_parques.xlsx", n_max = 0))

# Definir tipos de importação
tipos_colunas <- ifelse(
  nomes_colunas %in% c("horario_abertura", "horario_fechamento"),
  "numeric",
  "text"
)

# Importar planilha
lista_parques <- read_excel(
  "lista_parques.xlsx",
  col_types = tipos_colunas
)

# Separar latitude e longitude
lista_parques <- lista_parques %>%
  separate(
    latitude_longitude,
    into = c("latitude", "longitude"),
    sep = ",\\s*",
    remove = FALSE
  ) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

lista_parques = lista_parques %>%
  mutate(
  cod_mun=as.factor(cod_mun),
  municipio=as.factor(municipio),
  pista_caminhada=as.factor(pista_caminhada),
  outras_estruturas=as.factor(outras_estruturas),
  grades=as.factor(grades),
  incluir=as.factor(incluir)
  ) %>%
  filter(
    incluir == "s"
  )

lista_parques <- lista_parques %>%
  mutate(
    id_parque = row_number()
  )

##dados por setor - renda e população
renda <- read_tracts(
  year = 2022,
  dataset = "ResponsavelRenda"
)
 
renda_df <- renda %>%
  select(
    code_tract,
    code_muni,
    name_muni,
    V06002,
    V06004
  ) %>%
  collect()

  municipios_estudo <- as.numeric(
  as.character(
    unique(lista_parques$cod_mun)
  )
)

renda_df <- renda_df %>%
  filter(code_muni %in% municipios_estudo) %>%
  rename(
    populacao = V06002,
    renda_responsavel = V06004
  )

setores_sf <- map_dfr(
  municipios_estudo,
  ~ read_census_tract(
      code_tract = .x,
      year = 2022
    )

)

#juncao renda+setores

setores_sf <- setores_sf %>%
  mutate(
    code_tract = as.character(code_tract)
  )
setores_sf <- setores_sf %>%
  left_join(
    renda_df,
    by = "code_tract"
  )



# =========================================================
# TRANSFORMAR PARQUES EM OBJETO ESPACIAL
# =========================================================

parques_sf <- st_as_sf(
  lista_parques,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# =========================================================
# PROJETAR PARA SISTEMA MÉTRICO
# =========================================================

parques_sf <- st_transform(
  parques_sf,
  5880
)

setores_sf <- st_transform(
  setores_sf,
  5880
)

# =========================================================
# CRIAR BUFFER DE 1 KM
# =========================================================

buffers_1km <- st_buffer(
  parques_sf,
  dist = 1000
)

# =========================================================
# ÁREA TOTAL DOS SETORES
# =========================================================

setores_sf$area_setor <- st_area(setores_sf)

# =========================================================
# INTERSEÇÃO BUFFER × SETOR
# =========================================================

intersec <- st_intersection(
  buffers_1km,
  setores_sf
)

# =========================================================
# ÁREA INTERCEPTADA
# =========================================================

intersec$area_intersec <- st_area(intersec)

# =========================================================
# FRAÇÃO DO SETOR DENTRO DO BUFFER
# =========================================================

intersec$frac_setor <- as.numeric(
  intersec$area_intersec /
    intersec$area_setor
)

# =========================================================
# POPULAÇÃO ESTIMADA DENTRO DO BUFFER
# =========================================================

intersec$pop_buffer <- with(
  intersec,
  populacao * frac_setor
)

# =========================================================
# RESUMO POR PARQUE
# =========================================================

resultado_renda <- intersec %>% ###aqui esta somando proporcionalmente a renda de cada setor, ponderada pela população dentro do buffer
  st_drop_geometry() %>%
  group_by(id_parque) %>%
  summarise(    
    setores_interceptados = n(),    
    setores_sem_pop = sum(
      is.na(populacao)
    ),    
    setores_sem_renda = sum(
      is.na(renda_responsavel)
    ),    
    populacao_buffer = sum(
      pop_buffer,
      na.rm = TRUE
    ),    
    renda_media_1km =
      sum(
        renda_responsavel *
          pop_buffer,
        na.rm = TRUE
      ) /
      sum(
        pop_buffer[
          !is.na(renda_responsavel)
        ],
        na.rm = TRUE
      )    
  )

# =========================================================
# ADICIONAR RESULTADO À TABELA DE PARQUES
# =========================================================

lista_parques <- lista_parques %>%
  left_join(
    resultado_renda,
    by = "id_parque"
  )



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


save(
  lista_parques,
  file = "lista_parques_com_renda.RData"
)
save(
  resultado_renda,
  file = "resultado_renda_parques.RData"
)
save(
  setores_sf,
  file = "setores_sf.RData"
)
save(
  parques_sf,
  file = "parques_sf.RData"
)
save(
    buffers_1km,
    file = "buffers_1km.RData"
)
save(renda_df, file = "renda_df.RData")

save(intersec, file = "intersec.RData")