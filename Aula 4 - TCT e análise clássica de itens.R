# Aula 4/5 - Teoria Clássica dos Testes e fidedignidade
# Script de exercícios em R
# Objetivos da parte prática:
# 1. Descrever itens com foco em TCT
# 2. Examinar frequência de categorias, médias e variabilidade
# 3. Observar estatísticas descritivas com psych::describe() e ltm::descript()
# 4. Calcular escores e índices de consistência interna com função de escoragem
# 5. Interpretar alpha, G6 e resultados detalhados de psych::alpha()

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(psych)
library(ltm)
library(devtools)

# Importar banco e dicionário ---------------------------------------------

dados <- read_excel("banco_questionario_ficticio_200casos.xlsx")

dicionario <- read_xlsx("dicionario.xlsx", 2) %>%
  as.data.frame()

# Carregar função de escoragem --------------------------------------------

source_url("https://raw.githubusercontent.com/hauck-nf/codes/main/analyze_psychometrics_hierarchical.R")

# Inspeção inicial --------------------------------------------------------

glimpse(dados)

glimpse(dicionario)

# Exercício 1 - Descrição inicial dos itens com psych::describe() ---------

# Estatísticas descritivas dos itens
dados %>%
  dplyr::select(starts_with("bfi")) %>%
  psych::describe()

# Tabela resumida mais legível
dados %>%
  dplyr::select(starts_with("bfi")) %>%
  psych::describe() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "itemcode") %>%
  dplyr::select(itemcode, mean, sd, min, max, skew, kurtosis) %>%
  dplyr::mutate(
    dplyr::across(c(mean, sd, skew, kurtosis), ~ round(.x, 2))
  )

# Perguntas para discussão:
# 1. Há itens com médias muito altas ou muito baixas?
# 2. Há itens com pouca variabilidade?
# 3. O que isso pode sugerir sobre o comportamento dos itens na TCT?

# Exercício 2 - Frequência das categorias de resposta ---------------------

# Frequências por item
tabela_freq_prop <- dados %>%
  dplyr::select(starts_with("bfi")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "itemcode",
    values_to = "resposta"
  ) %>%
  count(itemcode, resposta) %>%
  group_by(itemcode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = resposta,
    values_from = c(n, prop),
    values_fill = 0
  )%>%
  dplyr::mutate(
    dplyr::across(c(prop_1, prop_2, prop_3, prop_4, prop_5, prop_NA), ~ round(.x, 2))
  )

tabela_freq_prop %>%
  print(n = 30)

# Perguntas para discussão:
# 1. Quais itens parecem mais "fáceis" de endossar?
# 2. Quais itens parecem mais concentrados em poucas categorias?
# 3. Que efeito isso pode ter na fidedignidade?

# Exercício 3 - Estatísticas dos itens com ltm::descript() ----------------

# A função descript() fornece estatísticas úteis em contexto de TCT/IRT
dados %>%
  dplyr::select(starts_with("bfi")) %>%
  ltm::descript()

# Observação:
# Compare a saída de ltm::descript() com psych::describe()

# Perguntas para discussão:
# 1. Que semelhanças e diferenças aparecem em relação ao psych::describe()?
# 2. Que tipo de descrição parece mais diretamente útil para avaliação de itens?

# Exercício 4 - Número de itens por escala --------------------------------

# Contar quantos itens há em cada escala
dicionario %>%
  count(scale, sort = TRUE)

# Contar quantos itens há em cada higher_order
dicionario %>%
  count(higher_order, sort = TRUE)

# Perguntas para discussão:
# 1. Quais escalas têm número suficiente de itens para cálculo de consistência interna?
# 2. Por que escalas muito curtas exigem cautela na interpretação da fidedignidade?

# Exercício 5 - Rodar a função de escoragem -------------------------------

resultado <- analyze_psychometrics_hierarchical(
  data = dados,
  dictionary = dicionario,
  min_items_per_scale = 2
)

# Ver elementos do objeto de saída
names(resultado)

# Ver escores calculados
resultado$scores

# Ver tabela resumida de fidedignidade
resultado$reliability

# Ver escalas eventualmente puladas
resultado$skipped_constructs

# Perguntas para discussão:
# 1. Quais escores foram calculados?
# 2. Houve construtos pulados por insuficiência de itens?
# 3. Por que isso é importante em TCT?

# Exercício 6 - Interpretar alpha e G6 ------------------------------------

# Tabela de confiabilidade
resultado$reliability %>%
  arrange(level, scale)

# Selecionar colunas mais importantes
resultado$reliability %>%
  select(instrument, level, scale, n_items, alpha, G6, alpha_raw, alpha_std, average_r)

# Perguntas para discussão:
# 1. Quais escalas apresentam maior alpha?
# 2. Há diferenças entre alpha e G6?
# 3. O número de itens parece influenciar a fidedignidade?

# Exercício 7 - Análise detalhada de uma escala com psych::alpha() --------

# Ver nomes disponíveis na análise de TCT
names(resultado$Classical_Test_Theory_Analysis$scale)

# Exemplo: acessar uma escala específica
resultado$Classical_Test_Theory_Analysis$scale[["BFI2S__Extroversao"]]

# Estatísticas totais
resultado$Classical_Test_Theory_Analysis$scale[["BFI2S__Extroversao"]]$total

# Estatísticas dos itens
resultado$Classical_Test_Theory_Analysis$scale[["BFI2S__Extroversao"]]$item.stats

# Efeito de retirar cada item
resultado$Classical_Test_Theory_Analysis$scale[["BFI2S__Extroversao"]]$alpha.drop

# Perguntas para discussão:
# 1. O alpha da escala parece aceitável?
# 2. Algum item parece prejudicar a consistência interna?
# 3. O que significa interpretar alpha.drop?

# Exercício 8 - Repetir para fatores de ordem superior --------------------

# Ver nomes disponíveis em higher_order
names(resultado$Classical_Test_Theory_Analysis$higher_order)

# Exemplo: acessar um fator amplo
resultado$Classical_Test_Theory_Analysis$higher_order[["BFI2S__Estabilidade_Emocional"]]

# Estatísticas totais
resultado$Classical_Test_Theory_Analysis$higher_order[["BFI2S__Estabilidade_Emocional"]]$total

# Estatísticas dos itens
resultado$Classical_Test_Theory_Analysis$higher_order[["BFI2S__Estabilidade_Emocional"]]$item.stats

# Efeito de retirar cada item
resultado$Classical_Test_Theory_Analysis$higher_order[["BFI2S__Estabilidade_Emocional"]]$alpha.drop

# Perguntas para discussão:
# 1. A fidedignidade do fator amplo é maior do que a de alguma escala específica?
# 2. O aumento do número de itens parece ter efeito sobre o alpha?

# Exercício 9 - Resumo final da aula --------------------------------------

# Montar um quadro síntese das escalas calculadas
resultado$reliability %>%
  mutate(
    interpretacao = case_when(
      alpha >= .90 ~ "muito alta",
      alpha >= .80 ~ "alta",
      alpha >= .70 ~ "adequada",
      alpha >= .60 ~ "limítrofe",
      TRUE ~ "baixa"
    )
  ) %>%
  dplyr::select(instrument, level, scale, n_items, alpha, G6, interpretacao)
