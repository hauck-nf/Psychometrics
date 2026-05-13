# Aula - EFA e avaliação da dimensionalidade
# Script de exercícios em R
# Sequência lógica:
# 1. Selecionar os itens da análise
# 2. Investigar a dimensionalidade
# 3. Estimar a solução final
# 4. Interpretar cargas, comunalidades e correlações fatoriais
# 5. Redigir uma síntese curta

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(psych)
library(dplyr)
library(tibble)#checar se precisar de instalação
library(gt)#checar se precisar de instalação

# Importar dados ----------------------------------------------------------
setwd("C:/Users/hauck/OneDrive/Main Drive/USF/Aulas/2026/Psicometria/Recursos")
dados <- read_excel("banco_questionario_ficticio_200casos.xlsx")
item_dic <- read_excel("item_dic.xlsx")

# Exercício 1 - Qualidade dos dados e decisão sobre número de fatores ---------------------------

bfi_items <- item_dic %>%
  dplyr::filter(instrument == "big-five-inventory-2") %>%
  dplyr::select(itemcode) %>%
  dplyr::pull()

dados %>%
  dplyr::select(all_of(bfi_items)) %>%
  KMO()

dados %>%
  dplyr::select(all_of(bfi_items)) %>%
  psych::fa.parallel(
    cor = "poly"
  )

dados %>%
  dplyr::select(all_of(bfi_items)) %>%
  psych::VSS()

# Perguntas para discussão:
# 1. Quantos fatores parecem defensáveis?
# 2. A solução faz sentido teoricamente?
# 3. A decisão depende apenas do parallel analysis?

# Exercício 2 - Estimação da solução final -------------------------------

efa_final <- dados %>%
  dplyr::select(all_of(bfi_items)) %>%
  psych::fa(
    nfactors = 5,
    fm = "wls",
    rotate = "oblimin",
    cor = "poly"
  )

efa_final

# Exercício 3 - Matriz fatorial com texto dos itens -----------------------

cargas <- efa_final$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "itemcode") %>%
  dplyr::left_join(
    item_dic %>%
      dplyr::select(itemcode, item_text_port),
    by = "itemcode"
  ) %>%
  dplyr::relocate(item_text_port, .before = itemcode) %>%
  dplyr::mutate(
    dplyr::across(-c(item_text_port, itemcode), ~ round(.x, 2))
  )

comunalidades <- tibble(
  itemcode = names(efa_final$communality),
  h2 = round(efa_final$communality, 2),
  u2 = round(efa_final$uniquenesses, 2)
)

tabela_efa <- cargas %>%
  dplyr::left_join(comunalidades, by = "itemcode")

fator_cols <- setdiff(names(tabela_efa), c("item_text_port", "itemcode", "h2", "u2"))

tabela_efa %>%
  gt() %>%
  tab_style(
    style = cell_text(color = "red"),
    locations = lapply(fator_cols, function(col) {
      cells_body(
        columns = all_of(col),
        rows = abs(.data[[col]]) < .30
      )
    })
  )

# Perguntas para discussão:
# 1. Quais itens apresentam carga principal mais alta?
# 2. Há cross-loadings relevantes?
# 3. Quais itens têm comunalidade baixa?
# 4. A solução parece interpretável?

# Exercício 4 - Correlação entre fatores ---------------------------------

efa_final$Phi %>%
  round(2)

# Perguntas para discussão:
# 1. Os fatores parecem correlacionados?
# 2. Faz sentido ter usado rotação oblíqua?

# Exercício 5 - Alocação principal dos itens ------------------------------

tabela_fator_principal <- efa_final$loadings %>%
  unclass() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "itemcode") %>%
  tidyr::pivot_longer(
    cols = -itemcode,
    names_to = "fator",
    values_to = "carga"
  ) %>%
  dplyr::group_by(itemcode) %>%
  dplyr::slice_max(order_by = abs(carga), n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    item_dic %>%
      dplyr::select(itemcode, item_text_port),
    by = "itemcode"
  ) %>%
  dplyr::select(item_text_port, itemcode, fator, carga) %>%
  dplyr::mutate(carga = round(carga, 2))

tabela_fator_principal %>%
  print(n = 30)

# Perguntas para discussão:
# 1. Cada item parece pertencer claramente a um fator?
# 2. Há itens com alocação ambígua?
# 3. Há coerência substantiva entre item e fator principal?

# Exercício 6 - Índices globais da solução -------------------------------

tibble(
  RMSR = round(efa_final$rms, 3),
  RMSEA = round(efa_final$RMSEA[1], 3),
  TLI = round(efa_final$TLI, 3)
)

# Exercício 7 - Síntese curta em formato de artigo ------------------------

# Tarefa final:
# Redigir 3 a 5 linhas com:
# 1. método utilizado (EFA, correlação policórica, extração WLS, rotação oblimin)
# 2. número de fatores retidos
# 3. comentário geral sobre a interpretabilidade da solução
# 4. comentário breve sobre cross-loadings e comunalidades

# Fechamento --------------------------------------------------------------

# Ideia central:
# Em itens ordinais, a sequência lógica da EFA envolve:
# tipo de item -> matriz de associação adequada -> dimensionalidade ->
# estimação final -> interpretação substantiva da solução.
