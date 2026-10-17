# OBJETIVO:
# Gera todos os gráficos

#define data do arquivo base
date_file <- "2026-09-09" 
{
  ####BIBLIOTECA####
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(vars)
  library(svars)
  library(TTR)
  library(tsibble)
  library(conflicted)
  library(plotly)
  library(seasonal)
  library(webshot)
  library(RCurl)
  library(slider)
  library(patchwork)
  library(tseries)
  library(urca)
  library(ggh4x)
  
  options(scipen = 999)
  
  conflict_prefer("layout", "plotly")
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  
  source("./function_export_plot.R")
  
}

#carrega bases com as IRF de cada modelo e cria dataframe unificado
irf_plots_data_VAR_apendice <- read_rds(file = ".\\Output\\irf_plots_data_VAR_apendice.rds")
irf_plots_data_VEC_apendice <- read_rds(file = ".\\Output\\irf_plots_data_VEC_apendice.rds")

ordem_variaveis <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
# ordem_variaveis <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO", "SELIC") #TESTE A
# ordem_variaveis <- c("PIB", "CONCESSOES", "PGTO", "SELIC", "TX_JUROS") #TESTE B


irf_plots_data_apendice <- rbind(irf_plots_data_VAR_apendice, irf_plots_data_VEC_apendice) |>   
  mutate(
    # 1. Primeiro convertemos impulse e variable para fatores com a ordem desejada
    impulse  = factor(impulse, levels = ordem_variaveis),
    variable = factor(variable, levels = ordem_variaveis),
    fonte    = recode(fonte, 
                      "bcb" = "Contabil", 
                      "bru" = "Estoque")
  ) |> 
  # 2. Ordenamos o data frame primeiro por variable (linhas) e depois por impulse (colunas)
  arrange(variable, impulse) |> 
  mutate(
    plot = paste0("IRF: ", impulse, " - ", variable), 
    
    # 3. Agora sim criamos o facet_label na ordem exata e extraímos unique() ordenado
    facet_label = paste0("epsilon[", impulse, "] %->% ", variable),
    facet_label = factor(facet_label, levels = unique(facet_label))
  )

#carrega bases com as IRF de cada modelo e cria dataframe unificado
irf_plots_data_VAR_selecionado <- read_rds(file = ".\\Output\\irf_plots_data_VAR_selecionado.rds")
irf_plots_data_VEC_selecionado <- read_rds(file = ".\\Output\\irf_plots_data_VEC_selecionado.rds")

irf_plots_data_selecionado <- rbind(irf_plots_data_VAR_selecionado, irf_plots_data_VEC_selecionado) |>   
  mutate(
    # 1. Primeiro convertemos impulse e variable para fatores com a ordem desejada
    impulse  = factor(impulse, levels = ordem_variaveis),
    variable = factor(variable, levels = ordem_variaveis),
    fonte    = recode(fonte, 
                      "bcb" = "Contabil", 
                      "bru" = "Estoque")
  ) |> 
  # 2. Ordenamos o data frame primeiro por variable (linhas) e depois por impulse (colunas)
  arrange(variable, impulse) |> 
  mutate(
    plot = paste0("IRF: ", impulse, " - ", variable), 
    
    # 3. Agora sim criamos o facet_label na ordem exata e extraímos unique() ordenado
    facet_label = paste0("epsilon[", impulse, "] %->% ", variable),
    facet_label = factor(facet_label, levels = unique(facet_label))
  )

# para teste
# plot_selected <- "IRF: SELIC - TX_JUROS"
# fonte_selected <- "Estoque"
# modelo_selected <- "VAR"

####COMPLETE IRF####
irf_plots_complete <- irf_plots_data_apendice |> select(modalidade, fonte, modelo) |> unique()

complete_irf <- function(modalidade_selected, fonte_selected, modelo_selected, data = irf_plots_data_apendice){
  
  
  
  if (modalidade_selected == "CC parcelado sem juros") {
    ncol_plot <- 4
  } else {
    ncol_plot <- 5
  }
  
  plot_complete_irf <- ggplot(irf_plots_data_apendice |> 
                                filter(fonte == fonte_selected & 
                                         modalidade == modalidade_selected &
                                         modelo == modelo_selected), aes(x = horizon, y = Mean)) +
    ggtitle(paste0("Funções impulso-resposta | ", modelo_selected, " | ", modalidade_selected, " | ", fonte_selected)) +
    geom_ribbon(aes(ymin = Low_05, ymax = Up_95), alpha = 0.1, fill = "darkgrey") +  # 90% CI (5% to 95%)
    geom_ribbon(aes(ymin = Low_10, ymax = Up_90), alpha = 0.3, fill = "darkgrey") +  # 80% CI (10% to 90%)
    geom_ribbon(aes(ymin = Low_16, ymax = Up_84), alpha = 0.5, fill = "darkgrey") +  # 68% CI (16% to 84%)
    geom_line(color = "black", size = 0.6) +  # Plot the mean IRF
    geom_hline(yintercept = 0, color = "red", size = 0.6) +
    labs(
      x = "Horizonte",
      y = "Resposta"
    ) +
    labs(x = "Horizonte",
         y = "Resposta") +
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),                    # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),  
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank(),
    )+
    facet_wrap(
      ~ facet_label, 
      scales = "free_y",
      labeller = label_parsed,
      ncol = ncol_plot # ajuste o número de colunas para bater com sua imagem (ex: 5 variáveis de impulso)
    ) +
    scale_x_continuous(breaks = seq(0, 12, 2))+  # Aumente o width para preencher o espaço
    theme_bw(base_size = 16) + # Tema de fundo branco
    theme(strip.text = element_text(size = 12)) # Aumenta o tamanh
  # plot_complete_irf
  
  message("IRF: Modelo: ", modelo_selected, " | Fonte: ", fonte_selected, " | Modalidade: ", modalidade_selected)
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\01_complete_IRF_", modalidade_selected,"_", fonte_selected,"_", modelo_selected, ".svg"), 
         plot = plot_complete_irf, width = 16, height = 10, dpi = 1000)
  
}

pwalk(irf_plots_complete, complete_irf)

####SELECTED PLOT - FONTE COMPARISON####

irf_plots <- expand.grid(
  plot = 
    c("IRF: SELIC - SELIC",
      "IRF: SELIC - TX_JUROS", 
      "IRF: SELIC - CONCESSOES",
      "IRF: SELIC - PGTO",
      "IRF: TX_JUROS - CONCESSOES",
      "IRF: TX_JUROS - PGTO"),
  fonte = c("bru", "bcb") ,
  modelo = c("VAR","VEC"))



irf_plots_comparison_all <- irf_plots |> select(-fonte, -modelo) |> unique()

free_y_selected_irf_comparison_all <- function(plot_selected, data = irf_plots_data_selecionado){
  irf_plots_data_selecionado$modalidade <- str_wrap(irf_plots_data_selecionado$modalidade, width = 15)
  
  plot_name <- str_remove(plot_selected, "IRF: ") |> str_replace(" - ", "-")
  # Extract the variables in the current group
  plot_irf_selected <- ggplot(irf_plots_data_selecionado |> 
                                filter(plot == plot_selected ), aes(x = horizon)) +
    geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)), size = 0.8) +  # Plot the mean IRF
    geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)), alpha = 0.1) +  # 68% CI (16% to 84%)
    geom_hline(yintercept = 0, color = "black", lwd = 0.6) +  # Aumente o width para preencher o espaço
    labs(x = "Horizonte",
         y = "Resposta") +
    theme_bw(base_size = 16) + # Tema de fundo branco
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),                    # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),  
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank(),
    )+
    facet_wrap(~modalidade, ncol = 4, scales = "free_y")  +  # Atribui as cores às modalidades
    scale_x_continuous(breaks = seq(0, 12, 2))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  # plot_irf_selected
  
  message("IRF: | Relação: ", plot_name)
  ggsave(
    filename = paste0(".\\Output\\Plots\\chapter_4_1\\02_free_axis_irf_comparison_all_", plot_name,".svg"),
    plot = plot_irf_selected, width = 16, height = 6, dpi = 1000)
  
}

pwalk(irf_plots_comparison_all, free_y_selected_irf_comparison_all)




fixed_y_selected_irf_comparison_all <- function(plot_selected, data = irf_plots_data_selecionado){
  irf_plots_data_selecionado$modalidade <- str_wrap(irf_plots_data_selecionado$modalidade, width = 15)
  
  plot_name <- str_remove(plot_selected, "IRF: ") |> str_replace(" - ", "-")
  # Extract the variables in the current group
  plot_irf_selected <- ggplot(irf_plots_data_selecionado |> 
                                filter(plot == plot_selected ), aes(x = horizon)) +
    geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)), size = 0.8) +  # Plot the mean IRF
    geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)), alpha = 0.1) +  # 68% CI (16% to 84%)
    geom_hline(yintercept = 0, color = "black", lwd = 0.6) +  # Aumente o width para preencher o espaço
    labs(x = "Horizonte",
         y = "Resposta") +
    theme_bw(base_size = 16) + # Tema de fundo branco
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),                    # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),  
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank(),
    )+
    facet_wrap(~modalidade, ncol = 4)  +  # Atribui as cores às modalidades
    scale_x_continuous(breaks = seq(0, 12, 2))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  # plot_irf_selected
  
  message("IRF: | Relação: ", plot_name)
  ggsave(
    filename = paste0(".\\Output\\Plots\\chapter_4_1\\03_fixed_axis_irf_comparison_all_", plot_name,".svg"),
    plot = plot_irf_selected, width = 16, height = 6, dpi = 1000)
  
}

pwalk(irf_plots_comparison_all, fixed_y_selected_irf_comparison_all)

####COMPLETE COMPARING####


irf_plots_modalidades <- irf_plots_data_apendice |> select(modalidade) |> unique()

complete_irf_data_comparing <- function(modalidade_selected = modalidade, data = irf_plots_data_apendice){
  
  if (modalidade_selected == "CC parcelado sem juros") {
    ncol_plot <- 4
  } else {
    ncol_plot <- 5
  }
  
  plot_complete_irf <- ggplot(irf_plots_data_apendice |> 
                                filter(modalidade == modalidade_selected), aes(x = horizon)) +
    ggtitle(paste0("Funções impulso-resposta | ", modalidade_selected)) +
    geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)), size = 0.8) +  # Plot the mean IRF
    geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)), alpha = 0.1) +  # 68% CI (16% to 84%)
    geom_hline(yintercept = 0, color = "black", size = 0.6) +
    labs(
      x = "Horizonte",
      y = "Resposta"
    ) +
    labs(x = "Horizonte",
         y = "Resposta") +
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),                    # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),  
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank(),
    )+
    facet_wrap(
      ~ facet_label, 
      scales = "free_y",
      labeller = label_parsed,
      ncol = ncol_plot # ajuste o número de colunas para bater com sua imagem (ex: 5 variáveis de impulso)
    ) +
    scale_x_continuous(breaks = seq(0, 12, 2))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  
  # plot_complete_irf
  
  message("IRF: Modalidade: ", modalidade_selected)
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\04_complete_irf_data_comparing_", modalidade_selected,".svg"), plot = plot_complete_irf, width = 16, height = 8, dpi = 1000)
  
}

pwalk(irf_plots_modalidades, complete_irf_data_comparing)

####IRF SELIC####
# variáveis de resposta (SELIC é sempre o impulso aqui, então sai do eixo de resposta)
ordem_variaveis_resposta <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO")

#####MODALIDADES CONVENCIONAIS####

ordem_modalidades_convencionais <- c(
  "Aquisição de bens", "Consignado", "Crédito pessoal não consignado", "Imobiliário")

irf_plots_selic_convencionais <- irf_plots_data_selecionado |> 
  filter(impulse == "SELIC", variable != "SELIC") |>  
  filter(modalidade %in% ordem_modalidades_convencionais) |> 
  mutate(
    variable   = factor(variable, levels = ordem_variaveis_resposta),
    modalidade = factor(modalidade, levels = ordem_modalidades_convencionais)
  )

plot_irf_selic_modalidades_convencionais <- ggplot(irf_plots_selic_convencionais, aes(x = horizon)) +
  geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)),
            linewidth = 0.8) +
  geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)),
              alpha = 0.1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(x = "Horizonte", y = "Resposta") +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    # panel.background = element_rect(fill = "white", color = "white"),
    # plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(size = 11)
  ) +
  ggh4x::facet_grid2(
    variable ~ modalidade,
    scales = "free_y",
    independent = "y",          # <- o que faz a mágica: cada painel com sua própria escala
    labeller = labeller(
      variable = c(
        TX_JUROS    = "Taxa de Juros",
        PIB         = "PIB",
        CONCESSOES  = "Concessões",
        PGTO        = "Pagamento"
      )
    ),
    switch = "y"
  ) +
  scale_x_continuous(breaks = seq(0, 12, 4)) +
  scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
  scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))

plot_irf_selic_modalidades_convencionais

message("IRF: choque de SELIC sobre modalidades convencionais")
ggsave(
  filename = ".\\Output\\Plots\\chapter_4_1\\04_irf_modalidades_convencionais.svg",
  plot = plot_irf_selic_modalidades_convencionais,
  width = 16, height = 8, dpi = 1000
)

#####MODALIDADES NÃO CONVENCIONAIS####

ordem_modalidades_nao_convencionais <- c(
  "CC parcelado sem juros", "CC parcelado", "CC rotativo", "Cheque especial"
)

irf_plots_selic_nao_convencionais <- irf_plots_data_selecionado |> 
  filter(impulse == "SELIC", variable != "SELIC") |>  
  filter(modalidade %in% ordem_modalidades_nao_convencionais) |> 
  mutate(
    variable   = factor(variable, levels = ordem_variaveis_resposta),
    modalidade = factor(modalidade, levels = ordem_modalidades_nao_convencionais)
  )

plot_irf_selic_modalidades_nao_convencionais <- ggplot(irf_plots_selic_nao_convencionais, aes(x = horizon)) +
  geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)),
            linewidth = 0.8) +
  geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)),
              alpha = 0.1) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(x = "Horizonte", y = "Resposta") +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_line(color = "grey90"),
    # panel.background = element_rect(fill = "white", color = "white"),
    # plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(size = 11)
  ) +
  ggh4x::facet_grid2(
    variable ~ modalidade,
    scales = "free_y",
    independent = "y",          # <- o que faz a mágica: cada painel com sua própria escala
    labeller = labeller(
      variable = c(
        TX_JUROS    = "Taxa de Juros",
        PIB         = "PIB",
        CONCESSOES  = "Concessões",
        PGTO        = "Pagamento"
      )
    ),
    switch = "y"
  ) +
  scale_x_continuous(breaks = seq(0, 12, 4)) +
  scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
  scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))

plot_irf_selic_modalidades_nao_convencionais

message("IRF: choque de SELIC sobre modalidades não convencionais")
ggsave(
  filename = ".\\Output\\Plots\\chapter_4_1\\04_irf_modalidades_nao_convencionais.svg",
  plot = plot_irf_selic_modalidades_nao_convencionais,
  width = 16, height = 8, dpi = 1000
)

####RESIDUALS PLOT####

VEC_resultados <- read_rds(file = ".\\Output\\VEC_resultados.rds")
VEC_outputs <- map(VEC_resultados, "result")
VEC_df_residuos <- bind_rows(lapply(VEC_outputs, `[[`, 3))|> mutate(modelo = "VEC")%>%
  rename(SELIC = `resids of SELIC`,           # Adjust column indices based on your actual data
         TX_JUROS = `resids of TX_JUROS`,
         PIB = `resids of PIB`,
         CONCESSOES = `resids of CONCESSOES`,
         PGTO = `resids of PGTO`)

VAR_resultados <- read_rds(file = ".\\Output\\VAR_resultados.rds")
VAR_outputs <- map(VAR_resultados, "result")
VAR_df_residuos <- bind_rows(lapply(VAR_outputs, `[[`, 3))|> mutate(modelo = "VAR")

residuals_data <- bind_rows(VAR_df_residuos, VEC_df_residuos)  |> 
  pivot_longer(-c(time, modalidade, metodo, fonte, modelo) , names_to = "variable", values_to = "residual") %>%
  mutate(variable = factor(variable, 
                           levels = c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")),
         
         fonte = recode(fonte, 
                        "bcb" = "Contabil", 
                        "bru" = "Estoque"))

residual_plot <- function(modalidade_selected, modelo_selected, data = residuals_data){
  
  plot_residual <- ggplot(residuals_data |> filter(modalidade == modalidade_selected) |> filter(modelo == modelo_selected), aes(x = time, y = residual)) +
    geom_col(aes(y = residual, fill = fonte), position = "dodge") +  # Plot the mean IRF
    
    geom_hline(yintercept = 0, linetype = "solid", color = "black", lwd = 0.5) +  # Aumente o width para preencher o espaço
    
    facet_wrap(~ variable, ncol = 1, scales = "free_y") + # Facets em coluna
    labs(title = paste0("Resíduos do modelo"), 
         subtitle = paste0("Modelo: ", modelo_selected,
                           "\nModalidade: ", modalidade_selected),
         x = "Data", y = "Resíduo") +
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_blank(),                    # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),  
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))## centraliza o título
  
  plot_residual
  
  message("IRF: Modelo: ", modelo_selected, " | Modalidade: ", modalidade_selected)
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\05_residuals_", modalidade_selected,"_", modelo_selected, ".svg"),
         plot = plot_residual, width = 16, height = 8, dpi = 1000)
}


pwalk(irf_plots_complete |> select(-fonte) |> unique(), residual_plot )


# sessionInfo()
