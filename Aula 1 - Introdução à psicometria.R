# Aula 1 - Introdução à psicometria
# Script de atividades em R
# Objetivos da parte prática:
# 1. Organizar o ambiente de trabalho
# 2. Simular um banco simples
# 3. Inspecionar a estrutura dos dados
# 4. Identificar variáveis demográficas e itens
# 5. Descrever o banco de forma psicométrica
# 6. Formular perguntas psicométricas plausíveis

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)

# Simulação de um banco simples -------------------------------------------
# Fixar semente para reprodutibilidade
set.seed(1234)

# Definir número de participantes
n <- 250

# Simular variáveis demográficas
idade <- round(rnorm(n, mean = 29, sd = 8))
sexo <- sample(
  x = c("mulher", "homem"),
  size = n,
  replace = TRUE,
  prob = c(0.60, 0.40)
)

escolaridade <- sample(
  x = c("graduação", "especialização", "mestrado", "doutorado"),
  size = n,
  replace = TRUE,
  prob = c(0.35, 0.25, 0.25, 0.15)
)

# Simular um traço latente simples
theta <- rnorm(n, mean = 0, sd = 1)

# Banco simulado didático: itens politômicos em escala Likert de 1 a 5
dados <- tibble(
  id = 1:n,
  idade = idade,
  sexo = sexo,
  escolaridade = escolaridade,
  item01 = pmin(pmax(round(3 + 0.9 * theta + rnorm(n, 0, 1.0)), 1), 5),
  item02 = pmin(pmax(round(3 + 0.8 * theta + rnorm(n, 0, 1.1)), 1), 5),
  item03 = pmin(pmax(round(3 + 0.7 * theta + rnorm(n, 0, 1.0)), 1), 5),
  item04 = pmin(pmax(round(3 + 0.6 * theta + rnorm(n, 0, 1.2)), 1), 5),
  item05 = pmin(pmax(round(3 + 0.9 * theta + rnorm(n, 0, 0.9)), 1), 5),
  item06 = pmin(pmax(round(3 + 0.5 * theta + rnorm(n, 0, 1.1)), 1), 5),
  item07 = pmin(pmax(round(3 + 0.8 * theta + rnorm(n, 0, 1.0)), 1), 5),
  item08 = pmin(pmax(round(3 + 0.7 * theta + rnorm(n, 0, 1.1)), 1), 5),
  item09 = pmin(pmax(round(3 + 0.6 * theta + rnorm(n, 0, 1.0)), 1), 5),
  item10 = pmin(pmax(round(3 + 0.8 * theta + rnorm(n, 0, 1.2)), 1), 5),
  item11 = pmin(pmax(round(3 + 0.7 * theta + rnorm(n, 0, 1.0)), 1), 5),
  item12 = pmin(pmax(round(3 + 0.9 * theta + rnorm(n, 0, 0.9)), 1), 5)
) %>%
  mutate(
    idade = if_else(idade < 18, 18, idade)
  )

# Introduzir alguns valores ausentes, para fins didáticos
dados <- dados %>%
  mutate(
    item03 = replace(item03, sample(1:n, 8), NA),
    item07 = replace(item07, sample(1:n, 10), NA),
    item11 = replace(item11, sample(1:n, 6), NA)
  )

# Visualizar o banco
dados

# Atividade 1 - Primeira inspeção do banco --------------------------------
# Ver estrutura geral
glimpse(dados)

# Dimensões do banco
dim(dados)

# Nomes das variáveis
names(dados)

# Resumo geral
skim(dados)

# Atividade 2 - Explorar mais sobre as variáveis -----------------------------
# Tabela com nome e classe de cada variável
tibble(
  variavel = names(dados),
  classe = map_chr(dados, ~ class(.x)[1])
)

# Contagem de valores ausentes por variável
dados %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "n_missing"
  ) %>%
  arrange(desc(n_missing))

# Número de categorias distintas por variável
dados %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "n_categorias"
  ) %>%
  arrange(n_categorias)

# Valores únicos observados em cada variável
valores_unicos_texto <- tibble(
  variavel = names(dados),
  classe = map_chr(dados, ~ class(.x)[1]),
  valores_unicos = map_chr(
    dados,
    ~ .x |>
      unique() |>
      sort(na.last = TRUE) |>
      as.character() |>
      replace(is.na(.), "NA") |>
      paste(collapse = ", ")
  )
)

valores_unicos_texto

# Atividade 3 - Separar itens do instrumento ------------------------------
# Selecionar apenas os itens
itens <- dados %>%
  select(starts_with("item"))

# Ver estrutura dos itens
glimpse(itens)

# Número de itens
ncol(itens)

# Nomes dos itens
names(itens)

# Atividade 4 - Resumo descritivo dos itens -------------------------------
# Resumo descritivo dos itens
itens %>%
  summarise(
    across(
      everything(),
      list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        media = ~ mean(.x, na.rm = TRUE),
        dp = ~ sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("item", ".value"),
    names_sep = "_(?=[^_]+$)"
  )


# Atividade 5 - Frequências de resposta -----------------------------------
# Frequências de um item específico
dados %>%
  count(item01) %>%
  mutate(prop = n / sum(n))

# Frequências de todos os itens
frequencias_itens <- itens %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "resposta"
  ) %>%
  count(item, resposta) %>%
  pivot_wider(
    names_from = resposta,
    values_from = n,
    values_fill = 0,
    names_prefix = "resp_"
  )

frequencias_itens

# Atividade 6 - Visualizar distribuições dos itens ------------------------
# Gráfico de barras para todos os itens
itens %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "resposta"
  ) %>%
  ggplot(aes(x = resposta)) +
  geom_bar() +
  facet_wrap(~ item, scales = "free_y")

# Atividade 7 - Relacionar itens e variáveis externas ---------------------
# Selecionar variáveis demográficas
demograficas <- dados %>%
  select(idade, sexo, escolaridade)

demograficas

# Verificar médias dos itens por sexo
dados %>%
  group_by(sexo) %>%
  summarise(
    across(starts_with("item"), ~ mean(.x, na.rm = TRUE))
  )

# Verificar médias dos itens por escolaridade
dados %>%
  group_by(escolaridade) %>%
  summarise(
    across(starts_with("item"), ~ mean(.x, na.rm = TRUE))
  )

# Atividade 8 - Criar uma função simples de resumo inicial ---------------
# Esta função ajuda a resumir rapidamente um banco para fins psicométricos
resumo_psicometrico_inicial <- function(data, seletor_itens) {
  
  itens <- data %>%
    select({{ seletor_itens }})
  
  resumo_itens <- itens %>%
    summarise(
      across(
        everything(),
        list(
          missing = ~ sum(is.na(.x)),
          categorias = ~ n_distinct(.x),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          media = ~ mean(.x, na.rm = TRUE),
          dp = ~ sd(.x, na.rm = TRUE)
        )
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("item", ".value"),
      names_sep = "_(?=[^_]+$)"
    )
  
  list(
    n_participantes = nrow(data),
    n_variaveis = ncol(data),
    n_itens = ncol(itens),
    resumo_itens = resumo_itens
  )
}

# Aplicar a função ao banco simulado
resultado_inicial <- resumo_psicometrico_inicial(
  data = dados,
  seletor_itens = starts_with("item")
)

resultado_inicial$n_participantes
resultado_inicial$n_variaveis
resultado_inicial$n_itens
resultado_inicial$resumo_itens