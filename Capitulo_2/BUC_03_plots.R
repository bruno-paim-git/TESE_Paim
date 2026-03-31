#Exercício para reproduzir com o pacote sfcr o artigo:
#DI BUCCHIANICO, Stefano; GALLO, Ettore; LOFARO, Antonino.
#Debt-credit Flows and Stocks in a Supermultiplier Model with Two Autonomous Demand Components: Consequences for Growth.
#Review of Political Economy, [s. l.], v. 36, n. 5, p. 1894–1914, 2024. 

####função para adicionar variáveis de análise nos modelos simulados####
process_model_df <- function(df_model) {
  df_model |>
    mutate(
      #serviço da dívida
      dsw_y = ((r+i)*Lw)/(w*Y),                                     
      #consumo em relação ao estoque de capital
      cw_k= Cw/lag(K),
      cr_k= Cr/lag(K),
      #consumo em relação à renda
      cw_y= Cw/Y,
      cr_y= Cr/Y,
      bw_y = Bw/Y,
      #nível de consumo
      log_Cw = log(Cw),
      #dívida em relação ao estoque de capital
      lw= Lw/K,
      #nível da renda
      log_y = log(Y),
      log_ydw = log(Ydw),
      gY = Y/lag(Y) -1,
      #patrimônio Líquido
      Vw_y = Vw/Y,
      Vr_y = Vr/Y,
      # SAVr_Y = SAVr/Y,
      Zw_y = Zw/Y,
      d = Bw/Lw,
      #estabilidade keynesiana
      # est_key = if ( (1- cr_v*ro_v*pi_v -cw_v*w_v) > h_v) {1} else {0}
    )|> 
    pivot_longer(cols = -any_of(c("period", "modelo")))
}

df_scenario1_TOTAL <- map_df(lista_modelos_baseline, process_model_df)

list_modelos <- grid_base_models$nome_modelo


####gera gráfico com as veriáveis ("h","d", "u")####
vars <- c("h","d", "u")

df_plot <-
  df_scenario1_TOTAL %>% 
    dplyr::filter(name %in% vars, period <= 100)
  
df_plot$modelo <- factor(df_plot$modelo, levels = list_modelos)
  df_plot$name <- factor(df_plot$name, levels = vars)
  
  labels_modelos <- c(
    "BUCCHIANICO_2024_a_BASELINE"   = "Base",
    "BUCCHIANICO_2024_b_schock_i"    = "Choque na taxa de juros (i)",
    "BUCCHIANICO_2024_c_schock_phi0" = "Choque no componente autônomo (φ0)"
  )
  
plot_simulation_bucchianico <-ggplot(df_plot, aes(x = period, y = value)) +
    geom_line(
      aes(
        linetype = modelo,
        color = modelo
      ),
      linewidth = 1
    ) +
    labs(
      x = "Horizonte",
      y = ""
    ) +
    theme_bw(base_size = 16) + # Tema de fundo branco
    theme(
      text = element_text(size = 18),       # aumenta todas as fontes
      axis.title = element_text(size = 18), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      legend.text = element_text(size = 20),
      panel.grid.minor = element_line(color = "grey90"),          # remove grades menores
      panel.grid.major.x = element_line(color = "grey90"),   # mantém apenas a grade principal em x
      panel.grid.major.y = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco no painel
      plot.background = element_rect(fill = "white", color = "white"),  # Fundo branco no gráfico
      legend.position = "bottom",
      legend.title = element_blank())+
    facet_wrap(~ name, scales = "free_y") +  
    scale_linetype_manual(
      values = setNames(
        c("solid", "dashed", "dashed"),
        list_modelos
      ),
      labels = labels_modelos
    ) +
    
  scale_color_manual(
    values = setNames(
      c("black",  "orange3", "green4"),
      list_modelos
    ),
    labels = labels_modelos
    )+
    theme(strip.text = element_text(size = 12)) 

plot_simulation_bucchianico

ggsave(filename = file.path("Output", paste0("plot_simulation_bucchianico", "_ggplot.png")), plot = plot_simulation_bucchianico, 
       width = 12, height = 4, dpi = 800 )

ggsave(filename = file.path("Output", paste0("plot_simulation_bucchianico", ".svg")), plot = plot_simulation_bucchianico, 
       width = 12, height = 4, dpi = 800 )


####gera gráfico com as veriáveis ("log_y","dsw_y", "cw_k", "lw")####

vars <- c("log_y","dsw_y", "cw_k", "lw")

df_plot <-
  df_scenario1_TOTAL %>% 
  dplyr::filter(name %in% vars, period <= 100)

df_plot$modelo <- factor(df_plot$modelo, levels = list_modelos)
df_plot$name <- factor(df_plot$name, levels = vars)


plot_simulation_2_bucchianico <-ggplot(df_plot, aes(x = period, y = value)) +
  geom_line(
    aes(
      linetype = modelo,
      color = modelo
    ),
    linewidth = 1
  ) +
  labs(
    x = "Horizonte",
    y = ""
  ) +
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
    legend.title = element_blank())+
  facet_wrap(~ name, scales = "free_y", ncol = 4) +  
  scale_linetype_manual(
    values = setNames(
      c("solid", "dashed", "dashed"),
      list_modelos
    ),
    labels = labels_modelos
  ) +
  
  scale_color_manual(
    values = setNames(
      c("black",  "orange3", "green4"),
      list_modelos
    ),
    labels = labels_modelos
  )+
  theme(strip.text = element_text(size = 12)) 

plot_simulation_2_bucchianico

ggsave(filename = file.path("Output", paste0("plot_simulation_2_bucchianico", "_ggplot.png")), plot = plot_simulation_2_bucchianico, 
       width = 12, height = 4, dpi = 800 )

ggsave(filename = file.path("Output", paste0("plot_simulation_2_bucchianico", ".svg")), plot = plot_simulation_2_bucchianico, 
       width = 12, height = 4, dpi = 800 )

