# Aula 2 - Teoria da medida, conceitos estatísticos e escalonamento
# Script de exercícios em R
# Objetivos da parte prática:
# 1. Simular um banco simples com diferentes formatos de distribuição
# 2. Descrever variáveis com estatísticas básicas
# 3. Examinar variância, assimetria e curtose
# 4. Comparar covariância e correlação
# 5. Transformar escores em z

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(psych)

# Simulação do banco ------------------------------------------------------

set.seed(2026)

n <- 250

dados <- tibble(
  sexo = sample(
    x = c("mulher", "homem"),
    size = n,
    replace = TRUE,
    prob = c(0.60, 0.40)
  ),
  idade = round(rnorm(n, mean = 28, sd = 8))
) %>%
  mutate(
    idade = if_else(idade < 18, 18, idade),
    
    # item01 - assimetria positiva
    item01 = sample(
      x = 1:5,
      size = n,
      replace = TRUE,
      prob = c(0.45, 0.25, 0.15, 0.10, 0.05)
    ),
    
    # item02 - assimetria negativa
    item02 = sample(
      x = 1:5,
      size = n,
      replace = TRUE,
      prob = c(0.05, 0.10, 0.15, 0.25, 0.45)
    ),
    
    # item03 - aproximadamente normal
    item03 = pmin(pmax(round(rnorm(n, mean = 3, sd = 1)), 1), 5),
    
    # item04 - aproximadamente normal
    item04 = pmin(pmax(round(rnorm(n, mean = 3, sd = 1)), 1), 5)
  ) %>%
  mutate(
    escore_total = item01 + item02 + item03 + item04
  )

dados

# Selecionar apenas as variáveis numéricas --------------------------------

dados_numericos <- dados %>%
  select(idade, item01, item02, item03, item04, escore_total)

dados_numericos

# Exercício 1 - Estatísticas descritivas básicas --------------------------

# A função describe() do psych já traz:
# n, mean, sd, median, min, max, range, skew, kurtosis e outras medidas

describe(dados_numericos)

# Se quiser uma tabela mais enxuta
tabela_descritiva <- describe(dados_numericos) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variavel") %>%
  select(
    variavel,
    n,
    mean,
    sd,
    var,
    min,
    max,
    skew,
    kurtosis
  )

tabela_descritiva

# Exercício 2 - Visualizar as distribuições -------------------------------

dados_numericos %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ variavel, scales = "free")

# Perguntas para discussão:
# 1. Qual item parece ter assimetria positiva?
# 2. Qual item parece ter assimetria negativa?
# 3. Quais variáveis parecem mais próximas de uma distribuição simétrica?

# Exercício 3 - Média, desvio-padrão e variância --------------------------

resumo_basico <- dados_numericos %>%
  summarise(
    across(
      everything(),
      list(
        media = ~ mean(.x, na.rm = TRUE),
        dp = ~ sd(.x, na.rm = TRUE),
        variancia = ~ var(.x, na.rm = TRUE),
        minimo = ~ min(.x, na.rm = TRUE),
        maximo = ~ max(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variavel", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

resumo_basico

# Perguntas para discussão:
# 1. Qual é a diferença entre desvio-padrão e variância?
# 2. O escore total tem amplitude maior do que os itens isolados?
# 3. O que isso implica para a interpretação do escore?

# Exercício 4 - Assimetria e curtose --------------------------------------

tabela_forma <- describe(dados_numericos) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variavel") %>%
  select(variavel, skew, kurtosis)

tabela_forma

# Perguntas para discussão:
# 1. Quais variáveis têm assimetria mais próxima de zero?
# 2. Qual item apresenta maior afastamento da simetria?
# 3. O que a curtose sugere sobre a concentração dos valores?

# Exercício 5 - Matriz de covariância -------------------------------------

matriz_cov <- dados_numericos %>%
  cov()

matriz_cov

# Exercício 6 - Matriz de correlação --------------------------------------

matriz_cor <- dados_numericos %>%
  cor()

matriz_cor

# Perguntas para discussão:
# 1. Qual a diferença conceitual entre covariância e correlação?
# 2. Por que a correlação é mais fácil de comparar entre pares de variáveis?
# 3. Por que o escore total tende a se correlacionar com os itens?

# Exercício 7 - Comparar covariância e correlação de um mesmo par ---------

dados_numericos %>%
  select(item03, item04) %>%
  cov()

dados_numericos %>%
  select(item03, item04) %>%
  cor()

# Exercício 8 - Transformação em escore z ---------------------------------

dados_z <- dados %>%
  mutate(
    z_idade = as.numeric(scale(idade)),
    z_item01 = as.numeric(scale(item01)),
    z_item02 = as.numeric(scale(item02)),
    z_item03 = as.numeric(scale(item03)),
    z_item04 = as.numeric(scale(item04)),
    z_escore_total = as.numeric(scale(escore_total))
  )

dados_z

# Verificar média e desvio-padrão aproximados dos escores z
dados_z %>%
  summarise(
    across(
      starts_with("z_"),
      list(
        media = ~ mean(.x, na.rm = TRUE),
        dp = ~ sd(.x, na.rm = TRUE),
        minimo = ~ min(.x, na.rm = TRUE),
        maximo = ~ max(.x, na.rm = TRUE)
      )
    )
  )

# Exercício 9 - Interpretar escore z --------------------------------------

dados_z %>%
  select(idade, z_idade, escore_total, z_escore_total) %>%
  slice(1:10)

# Perguntas para discussão:
# 1. O que significa um z = 0?
# 2. O que significa um z positivo?
# 3. O que significa um z negativo?
# 4. Em que sentido a transformação em z facilita a comparação entre variáveis?

# Exercício 10 - Mini-tarefa aplicada -------------------------------------

# Produzir uma tabela final resumida para interpretação
tabela_final <- describe(dados_numericos) %>%
  as.data.frame() %>%
  rownames_to_column(var = "variavel") %>%
  select(
    variavel,
    mean,
    sd,
    var,
    min,
    max,
    skew,
    kurtosis
  )

tabela_final

# Tarefa para os alunos:
# Escolher duas variáveis e responder:
# 1. Qual delas apresenta maior variabilidade?
# 2. Qual delas apresenta distribuição mais simétrica?
# 3. Como interpretar a correlação entre elas?
# 4. O que muda quando transformamos essas variáveis em escore z?