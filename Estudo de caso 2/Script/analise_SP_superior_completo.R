# =============================================================
#  ANÁLISE ESTATÍSTICA - NÍVEL SUPERIOR COMPLETO
#  Estado de São Paulo - Municípios - Censo 2022
# =============================================================
#
#  O que vamos analisar?
#  A coluna TOTAL_SC representa o % de pessoas com 18 anos ou mais
#  que completaram o Ensino Superior em cada município de SP.
#
#  Como rodar este script:
#  1. Instale o R em: https://cran.r-project.org/
#  2. Instale o RStudio (opcional, mas recomendado): https://posit.co/
#  3. Instale os pacotes necessários (só na primeira vez):
#     install.packages("readxl")
#     install.packages("ggplot2")
#     install.packages("dplyr")
# =============================================================


# ----- PASSO 1: Carregar os pacotes (ferramentas que vamos usar) -----

library(readxl)   # Para ler arquivos Excel
library(ggplot2)  # Para fazer gráficos bonitos
library(tidyverse)
library(dplyr)    # Para filtrar e organizar os dados


# ----- PASSO 2: Ler o arquivo Excel -----

# ATENÇÃO: Ajuste o caminho abaixo para onde o arquivo está no seu computador
# Exemplo Windows: "C:/Users/SeuNome/Downloads/Data_set_estudo_de_caso_2_tidy.xlsx"
# Exemplo Mac/Linux: "/home/seunome/Downloads/Data_set_estudo_de_caso_2_tidy.xlsx"


setwd("C:\\Users\\israe\\Downloads\\Estudo de caso 2")
dados_brutos <- read_excel("Data_set_estudo_de_caso_2_tidy.xlsx", skip = 1)

# ----- PASSO 3: Filtrar apenas São Paulo e só municípios -----

# TIPO == "MU" significa que é um município (e não estado ou região)
# UF == "SP" é o estado de São Paulo

sp_municipios <- dados_brutos %>%
  filter(UF == "SP", TIPO == "MU") %>%
  mutate(TOTAL_SC = as.numeric(TOTAL_SC))  # Garante que os números são números

cat("Municípios de SP encontrados:", nrow(sp_municipios), "\n\n")


# ----- PASSO 4: Calcular as estatísticas -----

# Extraímos só a coluna de interesse
superior_completo <- sp_municipios$TOTAL_SC

# Calculando cada estatística:
media     <- mean(superior_completo,   na.rm = TRUE)  # na.rm ignora valores vazios
mediana   <- median(superior_completo, na.rm = TRUE)
desvio_pd <- sd(superior_completo,     na.rm = TRUE)
minimo    <- min(superior_completo,    na.rm = TRUE)
maximo    <- max(superior_completo,    na.rm = TRUE)
amplitude <- maximo - minimo


# ----- PASSO 5: Mostrar os resultados -----

cat("============================================================\n")
cat(" RESULTADOS: % de pessoas com Nível Superior Completo em SP\n")
cat("============================================================\n")
cat(sprintf("  Nº de municípios analisados : %d\n",   length(superior_completo)))
cat(sprintf("  Média                       : %.2f%%\n", media))
cat(sprintf("  Mediana                     : %.2f%%\n", mediana))
cat(sprintf("  Desvio Padrão               : %.2f%%\n", desvio_pd))
cat(sprintf("  Mínimo                      : %.2f%%\n", minimo))
cat(sprintf("  Máximo                      : %.2f%%\n", maximo))
cat(sprintf("  Amplitude (máx - mín)       : %.2f%%\n", amplitude))
cat("============================================================\n\n")


# ----- PASSO 6: Encontrar os extremos (quem é quem?) -----

# Top 5 municípios com MAIS pessoas com ensino superior
top5 <- sp_municipios %>%
  arrange(desc(TOTAL_SC)) %>%
  slice(1:5) %>%
  select(NOME, TOTAL_SC)

# Bottom 5 municípios com MENOS pessoas com ensino superior
bottom5 <- sp_municipios %>%
  arrange(TOTAL_SC) %>%
  slice(1:5) %>%
  select(NOME, TOTAL_SC)

cat("--- Top 5: municípios com MAIS nível superior completo ---\n")
print(top5, row.names = FALSE)

cat("\n--- Bottom 5: municípios com MENOS nível superior completo ---\n")
print(bottom5, row.names = FALSE)
cat("\n")


# ----- PASSO 7: Gráfico de Histograma -----
#
#  O histograma mostra como os municípios se distribuem
#  em faixas de porcentagem. Cada barra = um grupo de municípios
#  com valores parecidos.

grafico_histograma <- ggplot(sp_municipios, aes(x = TOTAL_SC)) +

  # Desenha as barras do histograma (bins = número de barras)
  geom_histogram(
    bins  = 30,
    fill  = "#2E86AB",    # Cor azul para as barras
    color = "white",      # Borda branca entre barras
    alpha = 0.85          # Um pouco de transparência
  ) +

  # Linha vertical da MÉDIA (vermelho)
  geom_vline(
    xintercept = media,
    color      = "#E74C3C",
    linewidth  = 1.2,
    linetype   = "dashed"
  ) +

  # Linha vertical da MEDIANA (verde)
  geom_vline(
    xintercept = mediana,
    color      = "#FFA500",
    linewidth  = 1.2,
    linetype   = "dotdash"
  ) +

  # Rótulo da média
  annotate("text",
    x     = media + 1.5,
    y     = 70,
    label = paste0("Média\n", round(media, 1), "%"),
    color = "#E74C3C",
    size  = 3.5,
    fontface = "bold"
  ) +

  # Rótulo da mediana
  annotate("text",
    x     = mediana - 2,
    y     = 70,
    label = paste0("Mediana\n", round(mediana, 1), "%"),
    color = "#FFA500",
    size  = 3.5,
    fontface = "bold"
  ) +

  # Títulos e rótulos dos eixos
  labs(
    title    = "Distribuição do Nível Superior Completo nos Municípios de SP",
    subtitle = paste0(
      "644 municípios | Censo 2022 | ",
      "Desvio padrão: ", round(desvio_pd, 1), "% | ",
      "Amplitude: ", round(amplitude, 1), "%"
    ),
    x        = "% da população com Ensino Superior Completo",
    y        = "Número de municípios",
    caption  = "Fonte: IBGE - Censo Demográfico 2022"
  ) +

  # Tema visual limpo
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, color = "#7F8C8D"),
    axis.title    = element_text(color = "#2C3E50"),
    plot.caption  = element_text(color = "#95A5A6", size = 9)
  )

# Mostrar o gráfico na tela
print(grafico_histograma)


# ----- PASSO 8: Gráfico de Barras - Top 15 e Bottom 15 -----

# Juntar os extremos para comparação
extremos <- bind_rows(
  top5    %>% mutate(grupo = "Top 5 (mais desiguais acima)"),
  bottom5 %>% mutate(grupo = "Bottom 5 (menos acesso)")
) %>%
  mutate(NOME = reorder(NOME, TOTAL_SC))

grafico_barras <- ggplot(extremos, aes(x = NOME, y = TOTAL_SC, fill = grupo)) +

  geom_col(show.legend = TRUE, width = 0.7) +

  # Rótulos nas barras
  geom_text(
    aes(label = paste0(round(TOTAL_SC, 1), "%")),
    hjust = -0.1,
    size  = 3.5,
    color = "#2C3E50"
  ) +

  # Linha da média geral
  geom_hline(
    yintercept = media,
    linetype   = "dashed",
    color      = "#E74C3C",
    linewidth  = 0.8
  ) +

  coord_flip(clip = "off") +  # Barras horizontais, mais fácil de ler nomes

  scale_fill_manual(
    values = c("Top 5 (mais desiguais acima)" = "#2E86AB",
               "Bottom 5 (menos acesso)"       = "#E8A838"),
    name = ""
  ) +

  scale_y_continuous(
    limits = c(0, 55),
    labels = function(x) paste0(x, "%")
  ) +

  labs(
    title    = "Extremos da Desigualdade: Municípios de SP com Nível Superior Completo",
    subtitle = paste0("Linha vermelha tracejada = Média geral (", round(media, 1), "%)"),
    x        = NULL,
    y        = "% com Ensino Superior Completo",
    caption  = "Fonte: IBGE - Censo Demográfico 2022"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = "#2C3E50"),
    plot.subtitle = element_text(size = 10, color = "#7F8C8D"),
    legend.position = "bottom",
    plot.caption  = element_text(color = "#95A5A6", size = 9)
  )

print(grafico_barras)

cat("Análise concluída com sucesso!\n")
cat("Verifique os gráficos na aba 'Plots' do RStudio.\n")
