# =============================================================================
# ANÁLISE DESCRITIVA — SUPERIOR COMPLETO (TOTAL) POR UNIDADE DA FEDERAÇÃO
# Fonte: Censo 2022 – IBGE
# Indicador: TOTAL_SC (% pessoas 18+ com superior completo – Total)
# =============================================================================

# ── 0. Pacotes a ser instalados ───────────────────────────────────────────────
#Remova os jogos da velha para instalar e ler os pacotes.

#install.packages(c("readxl", "dplyr", "ggplot2", "scales", "forcats"))
#library(tidyverse)
#library(dplyr)
#library(readxl)
#library(dplyr)
#library(ggplot2)
#library(scales)
#library(forcats)

# ── 1. Leitura dos dados ──────────────────────────────────────────────────────
caminho <- "" # Coloque o caminho do arquivo entre as aspas

dados <- read_excel(caminho, sheet = "dados", skip = 2)

# ── 2. Filtrar apenas as 27 Unidades da Federação ────────────────────────────
# TIPO == "UF" e UF (coluna de sigla) == NA garante apenas os registros
# de nível estadual (exclui municípios homônimos de estados).

ufs <- dados |>
  filter(TIPO == "UF", is.na(UF)) |>
  select(COD_IBGE, NOME, TOTAL_SC)

# Regiões e Brasil para referência
regioes <- dados |>
  filter(TIPO == "GR") |>
  select(NOME, TOTAL_SC)

brasil <- dados |>
  filter(TIPO == "BR") |>
  pull(TOTAL_SC)

cat("=== Registros carregados ===\n")
cat("UFs:", nrow(ufs), "\n")
cat("Regiões:", nrow(regioes), "\n\n")

# ── 3. Mapa UF → Região ───────────────────────────────────────────────────────
regiao_uf <- tribble(
  ~NOME,                   ~REGIAO,
  "Rondônia",              "Norte",
  "Acre",                  "Norte",
  "Amazonas",              "Norte",
  "Roraima",               "Norte",
  "Pará",                  "Norte",
  "Amapá",                 "Norte",
  "Tocantins",             "Norte",
  "Maranhão",              "Nordeste",
  "Piauí",                 "Nordeste",
  "Ceará",                 "Nordeste",
  "Rio Grande do Norte",   "Nordeste",
  "Paraíba",               "Nordeste",
  "Pernambuco",            "Nordeste",
  "Alagoas",               "Nordeste",
  "Sergipe",               "Nordeste",
  "Bahia",                 "Nordeste",
  "Minas Gerais",          "Sudeste",
  "Espírito Santo",        "Sudeste",
  "Rio de Janeiro",        "Sudeste",
  "São Paulo",             "Sudeste",
  "Paraná",                "Sul",
  "Santa Catarina",        "Sul",
  "Rio Grande do Sul",     "Sul",
  "Mato Grosso do Sul",    "Centro-Oeste",
  "Mato Grosso",           "Centro-Oeste",
  "Goiás",                 "Centro-Oeste",
  "Distrito Federal",      "Centro-Oeste"
)

ufs <- ufs |> left_join(regiao_uf, by = "NOME")

# ── 4. Estatísticas descritivas ───────────────────────────────────────────────
stats <- ufs |>
  summarise(
    n          = n(),
    media      = mean(TOTAL_SC),
    mediana    = median(TOTAL_SC),
    desvio_pad = sd(TOTAL_SC),
    minimo     = min(TOTAL_SC),
    maximo     = max(TOTAL_SC),
    amplitude  = maximo - minimo
  )

cat("=================================================================\n")
cat("  ESTATÍSTICAS DESCRITIVAS — SUPERIOR COMPLETO (TOTAL) | UFs\n")
cat("=================================================================\n")
cat(sprintf("  N (estados)       : %d\n",   stats$n))
cat(sprintf("  Média             : %.2f%%\n", stats$media))
cat(sprintf("  Mediana           : %.2f%%\n", stats$mediana))
cat(sprintf("  Desvio padrão     : %.2f pp\n", stats$desvio_pad))
cat(sprintf("  Mínimo            : %.2f%%  (%s)\n",
            stats$minimo, ufs$NOME[which.min(ufs$TOTAL_SC)]))
cat(sprintf("  Máximo            : %.2f%%  (%s)\n",
            stats$maximo, ufs$NOME[which.max(ufs$TOTAL_SC)]))
cat(sprintf("  Amplitude         : %.2f pp\n", stats$amplitude))
cat(sprintf("  Brasil (referência): %.2f%%\n", brasil))
cat("=================================================================\n\n")

# ── 5. Ranking completo dos estados ──────────────────────────────────────────
cat("=== RANKING DOS ESTADOS (maior → menor) ===\n")
ranking <- ufs |>
  arrange(desc(TOTAL_SC)) |>
  mutate(posicao = row_number(),
         vs_brasil = TOTAL_SC - brasil,
         sinal = ifelse(vs_brasil >= 0, "+", ""))

ranking |>
  transmute(
    Pos  = posicao,
    UF   = NOME,
    Regiao = REGIAO,
    `SC (%)` = round(TOTAL_SC, 2),
    `vs Brasil (pp)` = paste0(sinal, round(vs_brasil, 2))
  ) |>
  print(n = 27)

# ── 6. Estatísticas por região ────────────────────────────────────────────────
cat("\n=== ESTATÍSTICAS POR REGIÃO ===\n")
stats_regiao <- ufs |>
  group_by(REGIAO) |>
  summarise(
    n_ufs      = n(),
    media      = round(mean(TOTAL_SC), 2),
    mediana    = round(median(TOTAL_SC), 2),
    desvio_pad = round(sd(TOTAL_SC), 2),
    minimo     = round(min(TOTAL_SC), 2),
    maximo     = round(max(TOTAL_SC), 2),
    amplitude  = round(maximo - minimo, 2),
    .groups = "drop"
  ) |>
  left_join(regioes |> rename(REGIAO = NOME, sc_regiao = TOTAL_SC), by = "REGIAO") |>
  arrange(desc(media))

print(stats_regiao)

# ── 7. Interpretação automática ───────────────────────────────────────────────
cat("\n=================================================================\n")
cat("  INTERPRETAÇÃO\n")
cat("=================================================================\n\n")

# 7.1 Variação geral
cv <- stats$desvio_pad / stats$media * 100
cat("── 7.1  Existe grande variação entre os estados?\n")
cat(sprintf(
  "  Desvio padrão de %.2f pp e amplitude de %.2f pp (de %.2f%% a %.2f%%)\n",
  stats$desvio_pad, stats$amplitude, stats$minimo, stats$maximo))
cat(sprintf("  Coeficiente de variação: %.1f%%\n", cv))
if (cv > 30) {
  cat("  → SIM. A variação é ALTA (CV > 30%). O acesso ao ensino superior\n")
  cat("    é profundamente desigual entre os estados brasileiros.\n\n")
} else if (cv > 15) {
  cat("  → MODERADA. Há heterogeneidade relevante entre os estados.\n\n")
} else {
  cat("  → BAIXA. Os estados apresentam padrão relativamente homogêneo.\n\n")
}

# 7.2 Destaques positivos (acima da média + 1 DP)
lim_sup <- stats$media + stats$desvio_pad
lim_inf <- stats$media - stats$desvio_pad

positivos <- ufs |> filter(TOTAL_SC >= lim_sup) |> arrange(desc(TOTAL_SC))
negativos <- ufs |> filter(TOTAL_SC <= lim_inf) |> arrange(TOTAL_SC)

cat("── 7.2  Estados que se destacam POSITIVAMENTE (≥ média + 1 DP):\n")
if (nrow(positivos) > 0) {
  for (i in seq_len(nrow(positivos))) {
    cat(sprintf("  • %s (%.2f%%) — %.2f pp acima da média nacional (%.2f%%)\n",
                positivos$NOME[i], positivos$TOTAL_SC[i],
                positivos$TOTAL_SC[i] - brasil, brasil))
  }
} else {
  cat("  Nenhum estado ultrapassa média + 1 DP.\n")
}

cat("\n── 7.3  Estados que se destacam NEGATIVAMENTE (≤ média – 1 DP):\n")
if (nrow(negativos) > 0) {
  for (i in seq_len(nrow(negativos))) {
    cat(sprintf("  • %s (%.2f%%) — %.2f pp abaixo da média nacional (%.2f%%)\n",
                negativos$NOME[i], negativos$TOTAL_SC[i],
                brasil - negativos$TOTAL_SC[i], brasil))
  }
} else {
  cat("  Nenhum estado fica abaixo de média – 1 DP.\n")
}

# 7.3 Comportamento regional
cat("\n── 7.4  O comportamento dos estados acompanha o da sua região?\n")
for (reg in unique(stats_regiao$REGIAO)) {
  ufs_reg <- ufs |> filter(REGIAO == reg) |> arrange(desc(TOTAL_SC))
  media_reg <- stats_regiao$media[stats_regiao$REGIAO == reg]
  sc_reg_oficial <- stats_regiao$sc_regiao[stats_regiao$REGIAO == reg]
  desvio_reg <- stats_regiao$desvio_pad[stats_regiao$REGIAO == reg]

  # Coerência: quantos estados ficam do mesmo lado (acima/abaixo) da média regional?
  mesmo_lado <- ufs_reg |>
    mutate(acima = TOTAL_SC > media_reg) |>
    summarise(pct = mean(acima)) |>
    pull(pct)

  cat(sprintf(
    "\n  [%s] SC médio da região (IBGE): %.2f%% | Média dos estados: %.2f%% | DP interno: %.2f pp\n",
    reg, sc_reg_oficial, media_reg, desvio_reg))

  outliers_reg <- ufs_reg |>
    filter(abs(TOTAL_SC - media_reg) > desvio_reg)

  if (nrow(outliers_reg) > 0) {
    outlier_nomes <- paste(sprintf("%s (%.1f%%)", outliers_reg$NOME, outliers_reg$TOTAL_SC),
                           collapse = ", ")
    cat(sprintf("  → Destaque(s) interno(s): %s\n", outlier_nomes))
  } else {
    cat("  → Estados internamente coesos (todos dentro de ±1 DP da média regional).\n")
  }
}

cat("\n=================================================================\n\n")

# ── 8. Gráficos ───────────────────────────────────────────────────────────────

cores_regiao <- c(
  "Norte"        = "#2196F3",
  "Nordeste"     = "#FF9800",
  "Sudeste"      = "#4CAF50",
  "Sul"          = "#9C27B0",
  "Centro-Oeste" = "#F44336"
)

# ── Gráfico 1: Barras horizontais ranqueadas ──────────────────────────────────
p1 <- ufs |>
  mutate(NOME = fct_reorder(NOME, TOTAL_SC)) |>
  ggplot(aes(x = TOTAL_SC, y = NOME, fill = REGIAO)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = brasil, linetype = "dashed", color = "gray30", linewidth = 0.7) +
  geom_vline(xintercept = stats$media, linetype = "dotted", color = "black", linewidth = 0.7) +
  annotate("text", x = brasil + 0.3, y = 1.5, label = paste0("Brasil\n", round(brasil,1), "%"),
           hjust = 0, size = 3, color = "gray30") +
  annotate("text", x = stats$media + 0.3, y = 4, label = paste0("Média UFs\n", round(stats$media,1), "%"),
           hjust = 0, size = 3, color = "black") +
  geom_text(aes(label = paste0(round(TOTAL_SC, 1), "%")),
            hjust = -0.1, size = 3, color = "gray20") +
  scale_fill_manual(values = cores_regiao) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = label_number(suffix = "%")) +
  labs(
    title    = "% de pessoas com Superior Completo por UF — Censo 2022",
    subtitle = "Indicador: TOTAL_SC | Linha tracejada = Brasil | Linha pontilhada = Média dos estados",
    x        = "Superior completo (%)",
    y        = NULL,
    fill     = "Região:",
    caption  = "Fonte: IBGE, Censo Demográfico 2022"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p1)

# ── Gráfico 2: Boxplot por região ─────────────────────────────────────────────
p2 <- ufs |>
  mutate(REGIAO = fct_reorder(REGIAO, TOTAL_SC, .fun = median, .desc = TRUE)) |>
  ggplot(aes(x = REGIAO, y = TOTAL_SC, fill = REGIAO)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = REGIAO), width = 0.15, size = 2.5, alpha = 0.9) +
  geom_text(aes(label = NOME),
            position = position_jitter(width = 0.15, seed = 42),
            size = 2.5, vjust = -0.8, color = "gray25") +
  geom_hline(yintercept = brasil, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  annotate("text", x = 0.5, y = brasil + 0.5,
           label = paste0("Brasil: ", round(brasil, 1), "%"),
           hjust = 0, size = 3.2, color = "gray40") +
  scale_fill_manual(values  = cores_regiao, guide = "none") +
  scale_color_manual(values = cores_regiao, guide = "none") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title    = "Distribuição de Superior Completo (%) por Região — UFs",
    subtitle = "Cada ponto é um estado | Linha tracejada = Brasil",
    x        = NULL,
    y        = "Superior completo (%)",
    caption  = "Fonte: IBGE, Censo Demográfico 2022"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p2)

# ── Gráfico 3: Desvio em relação à média nacional ────────────────────────────
p3 <- ufs |>
  mutate(
    desvio    = TOTAL_SC - brasil,
    sinal     = ifelse(desvio >= 0, "Acima", "Abaixo"),
    NOME      = fct_reorder(NOME, desvio)
  ) |>
  ggplot(aes(x = desvio, y = NOME, fill = sinal)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.8, color = "gray20") +
  geom_text(aes(label = paste0(ifelse(desvio >= 0, "+", ""), round(desvio, 1), " pp")),
            hjust = ifelse(ufs |> arrange(TOTAL_SC - brasil) |> pull(TOTAL_SC) >= brasil, -0.1, 1.1),
            size = 3, color = "gray20") +
  scale_fill_manual(values = c("Acima" = "#4CAF50", "Abaixo" = "#F44336")) +
  scale_x_continuous(labels = label_number(suffix = " pp"),
                     expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title    = "Desvio em relação ao Brasil — Superior Completo (Total)",
    subtitle = "Diferença em pontos percentuais em relação à média nacional (16,75%)",
    x        = "Desvio em relação ao Brasil (pp)",
    y        = NULL,
    fill     = NULL,
    caption  = "Fonte: IBGE, Censo Demográfico 2022"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "gray40", size = 10),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p3)

cat("Script concluído. Três gráficos gerados.\n")
