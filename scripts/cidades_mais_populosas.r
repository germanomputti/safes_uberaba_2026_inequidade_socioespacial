library(sidrar)
library(dplyr)
library(writexl)

root=""
pop <- get_sidra(
  api = "/t/4709/n6/all/v/93/p/2022"
)

top10_sp <- pop %>%
  mutate(
    cod_mun = as.character(`Município (Código)`),
    Valor = as.numeric(Valor)
  ) %>%
  filter(substr(cod_mun, 1, 2) == "35") %>%  # municípios de SP
  filter(cod_mun != "3550308") %>%           # exclui São Paulo
  arrange(desc(Valor)) %>%
  slice_head(n = 10) %>%
  select(
    municipio = Município,
    populacao = Valor,
    cod_mun=`Município (Código)`    
  )



write_xlsx(
  top10_sp,
  path = file.path(root, "top10_sp.xlsx")
)
