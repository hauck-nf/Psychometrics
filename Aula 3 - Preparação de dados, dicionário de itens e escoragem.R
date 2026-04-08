# Aula 3 - Preparação de dados, dicionário de itens e escoragem
# Script de exercícios em R
# Objetivos da parte prática:
# 1. Descrever os itens do banco
# 2. Verificar missingness
# 3. Calcular long string responding
# 4. Montar um dicionário mínimo
# 5. Calcular escores com função externa

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(psych)
library(careless)
library(devtools)

# Importar banco ----------------------------------------------------------

dados <- read_excel("banco_questionario_ficticio_200casos.xlsx")

# Inspeção inicial --------------------------------------------------------

glimpse(dados)

names(dados)

# Separar itens -----------------------------------------------------------

itens <- dados %>%
  select(starts_with("item"))

# Exercício 1 - Análise descritiva dos itens ------------------------------

# Estatísticas descritivas completas com psych::describe()
describe(itens)

# Tabela descritiva resumida
tabela_descritiva <- describe(itens) %>%
  as.data.frame() %>%
  rownames_to_column(var = "item") %>%
  select(
    item = var,
    mean,
    sd,
    var,
    min,
    max,
    skew,
    kurtosis
  )

tabela_descritiva

# Perguntas para discussão:
# 1. Quais itens parecem mais simétricos?
# 2. Há itens com média muito alta ou muito baixa?
# 3. Há itens com pouca variabilidade?

# Exercício 2 - Checagem de missingness ----------------------------------

# Número de missing por variável
missing_por_variavel <- dados %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "n_missing"
  ) %>%
  arrange(desc(n_missing))

missing_por_variavel

# Proporção de missing apenas nos itens
prop_missing_itens <- dados %>%
  summarise(across(starts_with("item"), ~ mean(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "prop_missing"
  ) %>%
  arrange(desc(prop_missing))

prop_missing_itens

# Teste de MCAR -----------------------------------------------------------

# Observação:
# MCAR pode ser testado.
# MAR não é testável diretamente a partir dos dados observados.

library(naniar)

teste_mcar <- mcar_test(itens)

teste_mcar

# Perguntas para discussão:
# 1. O banco tem poucos ou muitos dados ausentes?
# 2. O padrão de missing é compatível com MCAR?
# 3. O que não podemos concluir sobre MAR apenas com esse teste?

# Exercício 3 - Long string responding -----------------------------------

# Calcular long string para os itens
dados <- dados %>%
  mutate(
    longstring = itens %>%
      longstring()
  )

# Descrever long string
describe(dados$longstring)

# Ver os maiores valores
casos_longstring <- dados %>%
  select(id, longstring) %>%
  arrange(desc(longstring))

casos_longstring

casos_longstring %>%
  slice(1:10)

# Perguntas para discussão:
# 1. Quais casos apresentam maior sequência de respostas idênticas?
# 2. Um valor alto de long string basta, sozinho, para excluir um caso?

# Exercício 4 - Carregar função de escoragem e dicionário ------------------------------

source_url("https://raw.githubusercontent.com/hauck-nf/codes/main/analyze_psychometrics_hierarchical.R")
dicionario=read_xlsx("dicionario.xlsx",1)##ESTE DEVE SER O NOME DO DICIONÁRIO CRIADO E SALVO NO PC!!
dicionario<-as.data.frame(dicionario)

# Exercício 5 - Rodar a função --------------------------------------------

resultado <- analyze_psychometrics_hierarchical(
  data = dados,
  dictionary = dicionario
)

# Inspecionar o objeto de saída
names(resultado)

# Escores gerados
resultado$scores

# Base com escores
resultado$data_with_scores

# Confiabilidade
resultado$reliability

# Descrições
resultado$descriptives

# Auditoria
resultado$audit

# Perguntas para discussão:
# 1. Quais escores foram gerados?
# 2. Houve cálculo de escores de ordem superior?
# 3. Qual é a vantagem de usar banco bruto + dicionário + função?

# Fechamento --------------------------------------------------------------

# Ideia central da aula:
# Em psicometria, a análise não começa alterando o banco bruto.
# Ela começa organizando metadados, documentando regras de escoragem
# e produzindo um objeto de análise reproduzível.