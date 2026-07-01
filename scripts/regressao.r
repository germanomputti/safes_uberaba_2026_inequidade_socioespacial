library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(sf)
library(purrr)
library(binom)
library(ggplot2)
library(broom.mixed)
library(DHARMa)

setwd("")
rm(list=ls())

#abrir arquivos RData



load("buffers_1km.RData")
load("lista_parques_com_renda.RData")
load("parques_sf.RData")
load("renda_df.RData")
load("resultado_renda_parques.RData")
load("setores_sf.RData")
load("intersec.RData")



parques_por_setor <- intersec %>%
  distinct(id_parque, code_tract) %>%
  count(code_tract, name = "n_parques")

  dados_modelo <- renda_df %>%
  left_join(
    parques_por_setor,
    by = "code_tract"
  ) %>%
  mutate(
    n_parques = coalesce(n_parques, 0)
  ) %>%
  filter(
    !is.na(renda_responsavel),
    !is.na(populacao)
  )


  ###vamos olhar apenas presença/ausencia de parque
  dados_modelo <- dados_modelo %>%
  mutate(
    possui_parque = as.integer(n_parques > 0)
  )

#em 1000 reais
dados_modelo <- dados_modelo %>%
  mutate(
    renda_1000 = renda_responsavel / 1000
  )

dados_modelo <- dados_modelo %>%
  mutate(
    code_muni = as.factor(code_muni),
    possui_parque = as.integer(possui_parque),
    renda_1000 = as.numeric(renda_1000)
  )

modelo_log <- glm(
  possui_parque ~ renda_1000,
  family = binomial,
  data = dados_modelo
)


#cidade como efeito aleatório
modelo_misto <- glmer(
  possui_parque ~ renda_1000 +
    (1 | code_muni),
  family = binomial,
  data = dados_modelo
)

#### não rolou fazer contínuo. tentar fazer com 5 categorias




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
  ungroup()


  dados_modelo <- dados_modelo %>%
  left_join(
    renda_df_quintis %>%
      st_drop_geometry() %>%   # se existir geometria
      select(code_tract, quintil_pop),
    by = "code_tract"
  )

  dados_modelo <- dados_modelo %>%
  mutate(
    quintil_pop = factor(
      quintil_pop 
    )
  )


#modelo com quintis de renda

modelo_quintis <- glmer(
  possui_parque ~ quintil_pop +
    (1 | code_muni),
  family = binomial,
  data = dados_modelo
)






res <- simulateResiduals(modelo_quintis)

plot(res)
testDispersion(res)
testOutliers(res)
testUniformity(res)
testZeroInflation(res)


or_df <- tidy(
  modelo_quintis,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    IC_inf = exp(conf.low),
    IC_sup = exp(conf.high)
  )


  or_df <- tidy(
  modelo_quintis,
  effects = "fixed",
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    IC_inf = exp(conf.low),
    IC_sup = exp(conf.high),

    term = gsub("quintil_pop", "", term),

    term = factor(
      term,
      levels = c("Q2", "Q3", "Q4", "Q5")
    ),

    texto = sprintf(
      "%.2f (%.2f–%.2f)",
      OR,
      IC_inf,
      IC_sup
    )
  )


forest_plot <- ggplot(
  or_df,
  aes(
    x = OR,
    y = term
  )
) +

  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    linewidth = 0.8,
    colour = "grey50"
  ) +

  geom_errorbarh(
    aes(
      xmin = IC_inf,
      xmax = IC_sup
    ),
    height = 0.18,
    linewidth = 0.9
  ) +

  geom_point(
    size = 3.8
  ) +

  geom_text(
    aes(
      x = IC_sup + 0.05,
      label = texto
    ),
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +

  labs(
    x = "Odds ratio (Q1 como referência)",
    y = NULL
  ) +

  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.25))
  ) +

  coord_cartesian(
    clip = "off"
  ) +

  theme_classic(base_size = 16) +

  theme(
    axis.title.x = element_text(
      size = 16,
      face = "bold"
    ),

    axis.text.x = element_text(
      size = 14
    ),

    axis.text.y = element_text(
      size = 15,
      face = "bold"
    ),

    plot.margin = margin(
      5.5, 100, 5.5, 5.5
    )
  )

#teste de tendencia
dados_modelo <- dados_modelo %>%
  mutate(
    quintil_num = as.numeric(quintil_pop)
  )

  modelo_tendencia <- glmer(
  possui_parque ~ quintil_num +
    (1 | code_muni),
  family = binomial,
  data = dados_modelo
)


beta <- fixef(modelo_tendencia)["quintil_num"]

ic <- confint(
  modelo_tendencia,
  parm = "quintil_num",
  method = "Wald"
)

exp(c(beta, ic))


#proporcao observada de setores com parque em cada quintil
library(dplyr)

tabela_quintis <- dados_modelo %>%
  group_by(quintil_pop) %>%
  summarise(
    n_setores = n(),
    setores_com_parque = sum(possui_parque),
    proporcao = mean(possui_parque),
    .groups = "drop"
  ) %>%
  mutate(
    percentual = 100 * proporcao
  )

tabela_quintis


##proporcoes observadas


prop_observadas <- ggplot(
  tabela_quintis,
  aes(x = quintil_pop,
      y = percentual)
) +
  geom_col() +
  labs(
    x = "Quintil de renda",
    y = "% de setores com parque a até 1 km"
  ) +
  theme_minimal()



##probabilidade prevista pelo modelo de tendencia
novo <- data.frame(
  quintil_num = 1:5
)

eta <- predict(
  modelo_tendencia,
  newdata = novo,
  re.form = NA
)

novo$prob_prevista <- plogis(eta)

prop_prevista <- ggplot(
  novo,
  aes(x = factor(quintil_num),
      y = prob_prevista * 100)
) +
  geom_col() +
  labs(
    x = "Quintil de renda",
    y = "% previsto de setores com parque a até 1 km"
  ) +
  theme_minimal()


  ###tabela proporcoes de setores com e sem parque
  tabela_artigo <- dados_modelo %>%
  group_by(quintil_pop) %>%
  summarise(
    n_setores = n(),
    setores_com_parque = sum(possui_parque),
    percentual = round(100 * mean(possui_parque), 1),
    .groups = "drop"
  )


###ic 95 das proporcoes e graficos

library(binom)

tabela_artigo_ic <- dados_modelo %>%
  group_by(quintil_pop) %>%
  summarise(
    n = n(),
    eventos = sum(possui_parque),
    prop = eventos / n,
    .groups = "drop"
  ) %>%
  bind_cols(
    binom.confint(
      x = .$eventos,
      n = .$n,
      methods = "wilson"
    ) %>%
      select(lower, upper)
  ) %>%
  mutate(
    prop = 100 * prop,
    lower = 100 * lower,
    upper = 100 * upper
  )




quintil_proporcao_grafico=ggplot(
  tabela_artigo_ic,
  aes(x = quintil_pop,
      y = prop)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    width = .1
  ) +
  labs(
    x = "Quintil de renda",
    y = "% de setores com parque a até 1 km"
  ) +
  theme_classic()