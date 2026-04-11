
irf_plots_data_VAR <- read_rds(file = ".\\Output\\irf_plots_data_VAR.rds") |> select(-plot)|> mutate(horizon = horizon - 1)
irf_plots_data_VEC <- read_rds(file = ".\\Output\\irf_plots_data_VEC.rds")|> filter(horizon <= max(irf_plots_data_VAR$horizon))

ordem_variaveis <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")

irf_plots_data <- rbind(irf_plots_data_VEC, irf_plots_data_VAR) |>    
  mutate(plot = paste0("IRF: ", impulse, " - ", variable), 
         # criamos a string que o r entende como expressão matemática
         facet_label = paste0("epsilon[", impulse, "] %->% ", variable),
         # garantimos que a ordem das facetas siga a ordem original dos dados
         facet_label = factor(facet_label, levels = unique(facet_label)),
         variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         fonte = recode(fonte, 
                        "bcb" = "Contabil", 
                        "bru" = "Estoque"))


# write.csv2(irf_plots_data, file = ".\\Output\\write_irf_plots_data.csv")

# plot_selected <- "IRF: SELIC - TX_JUROS"
# fonte_selected <- "Estoque"
# modelo_selected <- "VAR"

####COMPLETE IRF####
irf_plots_complete <- irf_plots_data |> select(modalidade, fonte, modelo) |> unique()
# 
# irf_plots_complete <- expand.grid(
#   modalidade = 
#     unique(irf_plots_data$modalidade),
#   fonte = c("Estoque", "Contabil"),
#   modelo = c("VEC", "VAR")
# )


complete_irf <- function(modalidade_selected, fonte_selected, modelo_selected, data = irf_plots_data){
  
  
  
  if (modalidade_selected == "CC parcelado sem juros") {
    ncol_plot <- 4
  } else {
    ncol_plot <- 5
  }
  
  plot_complete_irf <- ggplot(irf_plots_data |> 
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
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\02_IRF_", modalidade_selected,"_", fonte_selected,"_", modelo_selected, ".svg"), 
         plot = plot_complete_irf, width = 16, height = 10, dpi = 1000)
  
}

pwalk(irf_plots_complete, complete_irf)

####SELECTED PLOT - FONTE COMPARISON - FREE Y####

irf_plots <- expand.grid(
  plot = 
    c("IRF: SELIC - SELIC",
      "IRF: SELIC - TX_JUROS", 
      "IRF: SELIC - CONCESSOES",
      "IRF: SELIC - PGTO",
      "IRF: PIB - CONCESSOES",
      "IRF: PIB - PGTO",
      "IRF: CONCESSOES - PGTO",
      "IRF: TX_JUROS - CONCESSOES",
      "IRF: TX_JUROS - PGTO"),
  fonte = c("bru", "bcb") ,
  modelo = c("VAR","VEC"))



irf_plots_comparison_all <- irf_plots |> select(-fonte, -modelo) |> unique()

free_y_selected_irf_comparison_all <- function(plot_selected, data = irf_plots_data){
  irf_plots_data$modalidade <- str_wrap(irf_plots_data$modalidade, width = 15)
  
  plot_name <- str_remove(plot_selected, "IRF: ") |> str_replace(" - ", "-")
  # Extract the variables in the current group
  plot_irf_selected <- ggplot(irf_plots_data |> 
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
    scale_x_continuous(breaks = seq(0, 12, 2), limits =  c(0,11))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  # plot_irf_selected
  
  message("IRF: | Relação: ", plot_name)
  ggsave(
    filename = paste0(".\\Output\\Plots\\chapter_4_1\\03_free_irf_comparison_all_", plot_name,".svg"),
    plot = plot_irf_selected, width = 16, height = 6, dpi = 1000)
  
}

pwalk(irf_plots_comparison_all, free_y_selected_irf_comparison_all)




fixed_y_selected_irf_comparison_all <- function(plot_selected, data = irf_plots_data){
  irf_plots_data$modalidade <- str_wrap(irf_plots_data$modalidade, width = 15)
  
  plot_name <- str_remove(plot_selected, "IRF: ") |> str_replace(" - ", "-")
  # Extract the variables in the current group
  plot_irf_selected <- ggplot(irf_plots_data |> 
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
    scale_x_continuous(breaks = seq(0, 12, 2), limits =  c(0,11))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  # plot_irf_selected
  
  message("IRF: | Relação: ", plot_name)
  ggsave(
    filename = paste0(".\\Output\\Plots\\chapter_4_1\\03_fixed_irf_comparison_all_", plot_name,".svg"),
    plot = plot_irf_selected, width = 16, height = 6, dpi = 1000)
  
}

pwalk(irf_plots_comparison_all, fixed_y_selected_irf_comparison_all)

  # Plot the mean IRF
  
# modalidade_selected <-"Consignado"


complete_irf_data <- function(modalidade_selected, data = irf_plots_data){
  
  plot_complete_irf <- ggplot(irf_plots_data |> 
                                filter(modalidade == modalidade_selected), aes(x = horizon)) +
    ggtitle(paste0("Funções impulso-resposta | ", modalidade_selected)) +
    geom_line(aes(y = Mean, color = fonte, linetype = modelo, group = interaction(fonte, modelo)), size = 0.8) +  # Plot the mean IRF
    geom_ribbon(aes(ymin = Low_16, ymax = Up_84, fill = fonte, group = interaction(fonte, modelo)), alpha = 0.1) +  # 68% CI (16% to 84%)
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
      ncol = 5 # ajuste o número de colunas para bater com sua imagem (ex: 5 variáveis de impulso)
    ) +
    scale_x_continuous(breaks = seq(0, 12, 2), limits =  c(0,11))+  # Aumente o width para preencher o espaço
    theme(strip.text = element_text(size = 12))  +
    scale_color_manual(values = c("Contabil" = "blue2", "Estoque" = "red3")) +
    scale_fill_manual(values  = c("Contabil" = "blue2", "Estoque" = "red3"))#
  
  # plot_complete_irf
  
  message("IRF: Modelo: ", modelo_selected, " | Fonte: ", fonte_selected, " | Modalidade: ", modalidade_selected)
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\02_IRF_", modalidade_selected,"_", fonte_selected,"_", modelo_selected, ".svg"), plot = plot_complete_irf, width = 16, height = 8, dpi = 1000)
  
}

pwalk(irf_plots_complete, complete_irf)


####RESIDUALS PLOT####

VEC_resultados <- read_rds(file = ".\\Output\\VEC_resultados.rds")
VEC_outputs <- map(VEC_resultados, "result")
VEC_df_residuos <- bind_rows(lapply(VEC_outputs, `[[`, 2))|> mutate(modelo = "VEC")%>%
  rename(SELIC = `resids of SELIC`,           # Adjust column indices based on your actual data
         TX_JUROS = `resids of TX_JUROS`,
         PIB = `resids of PIB`,
         CONCESSOES = `resids of CONCESSOES`,
         PGTO = `resids of PGTO`)

VAR_resultados <- read_rds(file = ".\\Output\\VAR_resultados.rds")
VAR_outputs <- map(VAR_resultados, "result")
VAR_df_residuos <- bind_rows(lapply(VAR_outputs, `[[`, 2))|> mutate(modelo = "VAR")

residuals_data <- bind_rows(VAR_df_residuos, VEC_df_residuos)  |> 
  pivot_longer(-c(time, modalidade, metodo, fonte, modelo) , names_to = "variable", values_to = "residual") %>%
  mutate(variable = factor(variable, 
                           levels = c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")),
         fonte = recode(fonte, 
                        "bcb" = "Contabil", 
                        "bru" = "Estoque"))

# Plotlibrary(ggplot2)
# modalidade_selected <-"Imobiliário"
# modelo_selected <- "VAR"

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
  ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\04_RESIDUALS_", modalidade_selected,"_", modelo_selected, ".svg"),
         plot = plot_residual, width = 16, height = 8, dpi = 1000)
}


pwalk(irf_plots_complete |> select(-fonte) |> unique(), residual_plot )


# sessionInfo()
