library(readxl)
library(dplyr)
library(tidyr)
library(geobr)
library(censobr)
library(sf)
library(purrr)



setwd("")
rm(list=ls())

#abrir arquivos RData



load("buffers_1km.RData")
load("lista_parques_com_renda.RData")
load("parques_sf.RData")
load("renda_df.RData")
load("resultado_renda_parques.RData")
load("setores_sf.RData")


#calculo percentil
calcula_percentil_ponderado <- function(renda_parque,
                                         renda_setores,
                                         populacao_setores) {

  dados <- data.frame(
    renda = renda_setores,
    pop = populacao_setores
  ) %>%
    filter(
      !is.na(renda),
      !is.na(pop)
    )

  if(nrow(dados) == 0 || is.na(renda_parque)) {
    return(NA_real_)
  }

  sum(dados$pop[dados$renda <= renda_parque]) /
    sum(dados$pop)
}


library(dplyr)

# garante mesmo tipo dos códigos
renda_df <- renda_df %>%
  mutate(code_muni = as.character(code_muni))

lista_parques <- lista_parques %>%
  mutate(cod_mun = as.character(cod_mun))



lista_parques <- lista_parques %>%
  rowwise() %>%
  mutate(
    percentil_renda_cidade = calcula_percentil_ponderado(
      renda_parque = renda_media_1km,
      renda_setores =
        renda_df$renda_responsavel[
          renda_df$code_muni == cod_mun
        ],

      populacao_setores =
        renda_df$populacao[
          renda_df$code_muni == cod_mun
        ]
    )
  ) %>%
  ungroup()


  lista_parques <- lista_parques %>%
  mutate(
    quintil_renda_cidade = case_when(
      percentil_renda_cidade <= 0.20 ~ "Q1",
      percentil_renda_cidade <= 0.40 ~ "Q2",
      percentil_renda_cidade <= 0.60 ~ "Q3",
      percentil_renda_cidade <= 0.80 ~ "Q4",
      TRUE                           ~ "Q5"
    )
  )


#summary(lista_parques$percentil_renda_cidade)

#table(lista_parques$quintil_renda_cidade)



######teste


renda_df_quintis <- renda_df %>%
  filter(
    !is.na(populacao),
    !is.na(renda_responsavel)
  ) %>%
  group_by(code_muni) %>%
  arrange(renda_responsavel, .by_group = TRUE) %>%
  mutate(
    pop_acum = cumsum(populacao),
    prop_acum = pop_acum / sum(populacao)
  ) %>%
  mutate(
    quintil = case_when(
      prop_acum <= 0.20 ~ "Q1",
      prop_acum <= 0.40 ~ "Q2",
      prop_acum <= 0.60 ~ "Q3",
      prop_acum <= 0.80 ~ "Q4",
      TRUE              ~ "Q5"
    )
  ) %>%
  ungroup()



  pop_quintis <- renda_df_quintis %>%
  group_by(quintil) %>%
  summarise(
    populacao = sum(populacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_pop = populacao / sum(populacao)
  )



parques_quintis <- lista_parques %>%
  count(quintil_renda_cidade) %>%
  rename(
    quintil = quintil_renda_cidade,
    parques = n
  ) %>%
  mutate(
    prop_parques = parques / sum(parques)
  )

comparacao <- left_join(
  pop_quintis,
  parques_quintis,
  by = "quintil"
) %>%
  mutate(
    razao_representacao = prop_parques / prop_pop
  )



chisq.test(
  x = parques_quintis$parques,
  p = rep(0.20, 5)
)

