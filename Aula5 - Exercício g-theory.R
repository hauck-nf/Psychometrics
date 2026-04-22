# Aula 5 - Exercício aplicado: reliability e generalizability theory
# Escala fictícia de ansiedade social
# Objetivos:
# 1. Simular um banco de dados com pacientes, itens e avaliadores
# 2. Estimar concordância entre avaliadores com kappa
# 3. Estimar ICC para escores totais
# 4. Estimar alpha por avaliador
# 5. Realizar uma análise simples de Generalizability Theory

library(dplyr)
library(tidyr)
library(purrr)
library(psych)
library(irr)#pacote novo, instalar!
library(lme4)#pacote novo, instalar!

# História do exercício ---------------------------------------------------
# Uma equipe clínica desenvolveu uma escala breve para avaliar ansiedade social.
# A escala possui 5 itens, pontuados de 0 a 3.
# Cada paciente foi avaliado por dois profissionais clínicos.
# O modelo de simulação assume que parte da variância decorre:
# - do nível latente de ansiedade social do paciente
# - da dificuldade/gravidade dos itens
# - do rigor dos avaliadores
# - de fontes residuais de interação e erro

# Simulação do banco ------------------------------------------------------

set.seed(2204)

n_pacientes <- 50
n_itens <- 5
n_avaliadores <- 2

pacientes <- tibble(
  paciente = factor(1:n_pacientes),
  theta = rnorm(n_pacientes, mean = 0, sd = 0.9)
)

itens <- tibble(
  item = factor(sprintf("item%02d", 1:n_itens)),
  dificuldade = c(-0.5, -0.2, 0, 0.3, 0.6)
)

avaliadores <- tibble(
  avaliador = factor(c("clinico_A", "clinico_B")),
  rigor = c(-0.10, 0.25)
)

dados_long <- tidyr::crossing(
  paciente = pacientes$paciente,
  item = itens$item,
  avaliador = avaliadores$avaliador
) %>%
  left_join(pacientes, by = "paciente") %>%
  left_join(itens, by = "item") %>%
  left_join(avaliadores, by = "avaliador") %>%
  mutate(
    efeito_paciente_item = rnorm(n(), 0, 0.35),
    efeito_paciente_avaliador = rnorm(n(), 0, 0.20),
    efeito_item_avaliador = rnorm(n(), 0, 0.15),
    erro = rnorm(n(), 0, 0.55),
    escore_continuo = 1.4 + 0.95 * theta + dificuldade + rigor +
      efeito_paciente_item + efeito_paciente_avaliador + efeito_item_avaliador + erro,
    resposta = cut(
      escore_continuo,
      breaks = c(-Inf, 0.4, 1.4, 2.4, Inf),
      labels = c(0, 1, 2, 3),
      ordered_result = TRUE
    ),
    resposta = as.numeric(as.character(resposta))
  ) %>%
  select(paciente, avaliador, item, resposta)

dados_long

# Se quiser salvar o banco:
# xlsx::write_xlsx(dados_long, "banco_ansiedade_social_long.xlsx", row.names = FALSE)

# Banco em formato wide por avaliador ------------------------------------

dados_wide <- dados_long %>%
  mutate(nome_coluna = paste0(item, "_", avaliador)) %>%
  select(paciente, nome_coluna, resposta) %>%
  pivot_wider(names_from = nome_coluna, values_from = resposta) %>%
  mutate(
    total_clinico_A = rowSums(dplyr::select(., matches("^item\\d{2}_clinico_A$")), na.rm = TRUE),
    total_clinico_B = rowSums(dplyr::select(., matches("^item\\d{2}_clinico_B$")), na.rm = TRUE)
  )

dados_wide

# Exercício 1 - Descrição inicial do banco --------------------------------

# Médias por item e avaliador

dados_long %>%
  group_by(avaliador, item) %>%
  summarise(
    media = mean(resposta, na.rm = TRUE),
    dp = sd(resposta, na.rm = TRUE),
    .groups = "drop"
  )

# Perguntas:
# 1. Algum avaliador parece mais rigoroso?
# 2. Algum item parece mais "difícil" ou mais "grave"?

# Exercício 2 - Kappa por item --------------------------------------------

kappa_por_item <- dados_long %>%
  pivot_wider(names_from = avaliador, values_from = resposta) %>%
  group_split(item) %>%
  map_dfr(
    ~ tibble(
      item = unique(.x$item),
      kappa_ponderado = irr::kappa2(
        .x %>% dplyr::select(clinico_A, clinico_B),
        weight = "squared"
      )$value
    )
  )

kappa_por_item

# Perguntas:
# 1. Em quais itens a concordância foi maior?
# 2. Há itens com concordância apenas modesta?

# Exercício 3 - ICC para escore total -------------------------------------

icc_totais <- irr::icc(
  dados_wide %>% dplyr::select(total_clinico_A, total_clinico_B),
  model = "twoway",
  type = "agreement",
  unit = "single"
)

icc_totais

# Perguntas:
# 1. O ICC sugere boa concordância entre avaliadores no escore total?
# 2. O que muda ao avaliar escore total, em vez de item individual?

# Exercício 4 - Alpha por avaliador ---------------------------------------

alpha_clinico_A <- psych::alpha(
  dados_wide %>% dplyr::select(matches("^item\\d{2}_clinico_A$"))
)

alpha_clinico_B <- psych::alpha(
  dados_wide %>% dplyr::select(matches("^item\\d{2}_clinico_B$"))
)

alpha_clinico_A$total
alpha_clinico_B$total

alpha_clinico_A$item.stats
alpha_clinico_B$item.stats

# Perguntas:
# 1. O alpha parece semelhante entre os dois avaliadores?
# 2. Há algum item com r.drop mais baixo?
# 3. A consistência interna parece aceitável para uma escala curta?

# Exercício 5 - Comparar kappa, ICC e alpha -------------------------------

resumo_confiabilidade <- tibble(
  indice = c(
    "Kappa médio dos itens",
    "ICC do escore total",
    "Alpha - clínico A",
    "Alpha - clínico B"
  ),
  valor = c(
    mean(kappa_por_item$kappa_ponderado, na.rm = TRUE),
    icc_totais$value,
    alpha_clinico_A$total$raw_alpha,
    alpha_clinico_B$total$raw_alpha
  )
)

resumo_confiabilidade

# Perguntas:
# 1. Esses índices respondem à mesma pergunta?
# 2. Em que contexto kappa seria mais útil?
# 3. Em que contexto ICC seria mais informativo?
# 4. O que alpha informa que kappa e ICC não informam diretamente?

# Exercício 6 - Generalizability Theory: G-study --------------------------

# Delineamento totalmente cruzado: paciente x item x avaliador
# Modelo com componentes aleatórios para paciente, item e avaliador

g_modelo <- lme4::lmer(
  resposta ~ 1 +
    (1 | paciente) +
    (1 | item) +
    (1 | avaliador) +
    (1 | paciente:item) +
    (1 | paciente:avaliador) +
    (1 | item:avaliador),
  data = dados_long,
  REML = TRUE
)

summary(g_modelo)

componentes_var <- as.data.frame(VarCorr(g_modelo)) %>%
  dplyr::select(grp, vcov) %>%
  mutate(grp = as.character(grp))

componentes_var

# Extrair componentes de variância
sigma_p  <- componentes_var %>% filter(grp == "paciente") %>% pull(vcov)
sigma_i  <- componentes_var %>% filter(grp == "item") %>% pull(vcov)
sigma_r  <- componentes_var %>% filter(grp == "avaliador") %>% pull(vcov)
sigma_pi <- componentes_var %>% filter(grp == "paciente:item") %>% pull(vcov)
sigma_pr <- componentes_var %>% filter(grp == "paciente:avaliador") %>% pull(vcov)
sigma_ir <- componentes_var %>% filter(grp == "item:avaliador") %>% pull(vcov)
sigma_e  <- componentes_var %>% filter(grp == "Residual") %>% pull(vcov)

n_i <- 5
n_r <- 2

# Coeficiente G para decisões relativas
G_coef <- sigma_p / (sigma_p + (sigma_pi / n_i) + (sigma_pr / n_r) + (sigma_e / (n_i * n_r)))

# Coeficiente Phi para decisões absolutas
Phi_coef <- sigma_p / (sigma_p +
  (sigma_i / n_i) +
  (sigma_r / n_r) +
  (sigma_pi / n_i) +
  (sigma_pr / n_r) +
  (sigma_ir / (n_i * n_r)) +
  (sigma_e / (n_i * n_r)))

tibble(
  G_coef = G_coef,
  Phi_coef = Phi_coef
)

# Perguntas:
# 1. Que fonte de variância parece mais importante?
# 2. A variância de avaliador é grande ou pequena?
# 3. O coeficiente G é maior que o Phi? Por quê?

# Exercício 7 - D-study simples -------------------------------------------

calc_g_phi <- function(n_itens, n_avaliadores) {
  tibble(
    n_itens = n_itens,
    n_avaliadores = n_avaliadores,
    G_coef = sigma_p / (sigma_p + (sigma_pi / n_itens) + (sigma_pr / n_avaliadores) + (sigma_e / (n_itens * n_avaliadores))),
    Phi_coef = sigma_p / (sigma_p +
      (sigma_i / n_itens) +
      (sigma_r / n_avaliadores) +
      (sigma_pi / n_itens) +
      (sigma_pr / n_avaliadores) +
      (sigma_ir / (n_itens * n_avaliadores)) +
      (sigma_e / (n_itens * n_avaliadores)))
  )
}

bind_rows(
  calc_g_phi(5, 2),
  calc_g_phi(8, 2),
  calc_g_phi(5, 3),
  calc_g_phi(8, 3)
)

# Perguntas:
# 1. O que parece melhorar mais a precisão: mais itens ou mais avaliadores?
# 2. Em uma decisão clínica, qual delineamento parece mais vantajoso?

# Tarefa final ------------------------------------------------------------
# Cada grupo deve responder:
# 1. Qual item apresentou maior concordância entre avaliadores?
# 2. O ICC do escore total foi satisfatório?
# 3. O alpha sugere consistência interna adequada?
# 4. Na análise de G-theory, quais fontes de erro foram mais importantes?
# 5. O que seria mais eficiente para aumentar a precisão: acrescentar itens ou avaliadores?
