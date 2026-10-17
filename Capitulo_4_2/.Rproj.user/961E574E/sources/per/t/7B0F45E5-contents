
####SCHOKS####

period_simulation <- 1000

####função para adicionar variáveis de análise nos modelos simulados
process_model_df <- function(df_model) {
  df_model |>
   # teste <-  df_model |>
    mutate(
      #serviço da dívida
      #serviço consumo induzido
      dsb_y = ((r_b+icc_b)*lag(Lb))/(Y),                                     
      #serviço consumo autônomo
      dsz_y = ((r_z+icc_z)*lag(Lz))/(Y),
      #serviço total
      ds_y = dsb_y + dsz_y,
      dsb_w = ((r_b+icc_b)*lag(Lb))/(W),                                     
      #serviço consumo autônomo
      dsz_w = ((r_z+icc_z)*lag(Lz))/(W),
      #serviço total
      ds_w = dsb_w + dsz_w,
      #consumo em relação ao estoque de capital
      cw_k= Cw/lag(K),
      cr_k= Cr/lag(K),
      #consumo em relação à renda
      cw_y= Cw/Y,
      cr_y= Cr/Y,
      cw_yw= Cw/W,
      cr_yr= Cr/Ydr,
      ciw_y = Ciw/Y,
      c_y = C/Y,
      bw_y = Bw/Y,
      zw_y = Zw/Y,
      zw_c = Zw/Cw,
      bw_c = Bw/Cw,
      ciw_c = Ciw/Cw,
      #nível de consumo
      log_Cw = log(Cw),
      log_Zw = log(Zw),
      log_Bw = log(Bw),
      log_Ciw = log(Ciw),
      log_Cr = log(Cr),
      #dívida em relação aos salários
      lb_w = Lb/W,
      lz_w = Lz/W,
      l_w= L/W,
      #dívida em relação a renda
      lb_y = Lb/Y,
      lz_y = Lz/Y,
      l_y = L/Y,
      #nível da renda
      log_y = log(Y),
      log_ydw = log(Ydw),
      gY = Y/lag(Y) -1,
      #patrimônio Líquido
      Vw_y = Vw/Y,
      Vr_y = Vr/Y,
      rel_l = Lb/Lz,
      # SAVr_Y = SAVr/Y,
      Zw_L = Zw/L,
      # Zw líquido
      #Balanced Growth Path
      Y_K = K/Y,
      Y_E = E/Y,
      Y_Lw = L/Y,
      Y_D = D/Y,
      #Consistência Financeira
      consistencia = delta_L - delta_D,
      #estabilidade keynesiana
      # est_key = if ( (1- cr_v*ro_v*pi_v -cw_v*(1-beta_v)*w_v) > h_v) {1} else {0}
    )|> 
    pivot_longer(cols = -any_of(c("period", "modelo")))
  
}

####função para gerar os subplots

# vars<- c("u", "h")

# var <- "u"
# idx <- "1"

gerar_subplot <- function(vars) {
  df_scenario1_filtered <- df_scenario1_TOTAL %>% 
    filter(name %in% vars) |> 
    filter(period <= 600)
  
  # Número de colunas
  ncols <- length(vars)
  nrows <- ceiling(length(vars) / ncols)
  

    plots_scenario1 <- map2(vars, seq_along(vars), function(var, idx) {
    df_var <- df_scenario1_filtered %>% filter(name == var)
    
    show_legend <- idx == 1  # só mostra a legenda no primeiro subplot
    
    plot_ly() %>%
      add_trace(
        data = df_var %>% filter(CHOQUE == list_CHOQUE[1]),
        x = ~period,
        y = ~value,
        type = 'scatter',
        mode = 'lines',
        line = list(dash = "solid", width = 2),
        name = list_CHOQUE[1],
        legendgroup = list_CHOQUE[1],
        showlegend = show_legend,
        color = I("black")
      ) %>%
      add_trace(
        data = df_var %>% filter(CHOQUE == list_CHOQUE[2]),
        x = ~period,
        y = ~value,
        type = 'scatter',
        mode = 'lines',
        line = list(dash = "dot", width = 2),
        name = list_CHOQUE[2],
        legendgroup = list_CHOQUE[2],
        showlegend = show_legend,
        color = I("#FC8D62")
      ) %>%
      layout(
        title = list(text = var, font = list(size = 10)),
        xaxis = list(title = ""),
        yaxis = list(title = "")
        # yaxis = list(dtick = 0.001)
        # yaxis = list(range = yaxis_range)
      )
  })
  
  # Combina os subplots
  p_model__scenario1 <- subplot(
    plots_scenario1,
    nrows = nrows,
    shareX = TRUE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.05
  ) %>%
    layout(
      legend = list(orientation = "h", 
                    x = 0.5, xanchor = "center", 
                    y = -0.3,   
                    font = list(size = 18)  # << tamanho da fonte da legenda
      ),
      margin = list(t = 70,
                    b = 0),
      
      # Aumenta fonte dos eixos
      xaxis = list(titlefont = list(size = 18), tickfont = list(size = 16)),
      xaxis2 = list(titlefont = list(size = 18), tickfont = list(size = 16)),
      xaxis3 = list(titlefont = list(size = 18), tickfont = list(size = 16)),
      
      yaxis = list(titlefont = list(size = 18), tickfont = list(size = 16)),
      yaxis2 = list(titlefont = list(size = 18), tickfont = list(size = 16)),
      yaxis3 = list(titlefont = list(size = 18), tickfont = list(size = 16))
    )
  
  # Criar as annotations (como título das subplots)
  annotations <- map2(
    seq_along(vars),
    vars,
    ~list(
      x = ((.x - 1) %% ncols) / ncols + 1 / (2 * ncols),
      y = 1 - floor((.x - 1) / ncols) / nrows +0.1,
      text = .y,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 20)
    )
  )
  
  # Adicionar títulos às subplots
  p_model__scenario1 %>%
    layout(
      title = "",
      annotations = annotations,
      template = "plotly_white"
    )
  
}

gerar_subplot_ggplot <- function(vars, periods = 600) {
  
  df_plot <- df_scenario1_TOTAL %>% 
    dplyr::filter(name %in% vars, period <= periods)
  
  df_plot$CHOQUE <- factor(df_plot$CHOQUE, levels = list_CHOQUE)
  df_plot$name <- factor(df_plot$name, levels = vars)
  
  
  ggplot(df_plot, aes(x = period, y = value)) +
    geom_line(
      aes(
        linetype = CHOQUE,
        color = CHOQUE
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
    facet_wrap(~ name, scales = "free_y") +  
    # Garante amplitude mínima de 0.001 no eixo Y
    scale_y_continuous(limits = function(lims) {
      amplitude <- diff(lims)
      min_amp <- 0.001
      if (amplitude < min_amp) {
        centro <- mean(lims)
        c(centro - min_amp / 2, centro + min_amp / 2)
      } else {
        lims
      }
    }) +
    scale_linetype_manual(
      values = setNames(
        c("solid", "dashed"),
        list_CHOQUE
      )
    ) +
    
    scale_color_manual(
      values = setNames(
        c("green4", "orange3"),
        list_CHOQUE
      )
    )+
    theme(strip.text = element_text(size = 12)) 
}

#função para salvar os subplots
salvar_subplot <- function(vars, filename, periods = 600) {
#plots ggplot
  plot_ggplot <- gerar_subplot_ggplot(vars, periods)
  
  ggsave(filename = file.path("Output", paste0(filename, "_ggplot.png")), plot = plot_ggplot, 
         width = 12, height = 4, dpi = 800 )
  
  ggsave(filename = file.path("Output", paste0(filename, ".svg")), plot = plot_ggplot, 
         width = 12, height = 4, dpi = 800 )
      }

calcular_lzw_y_equilibrio <- function(beta_v, phi0_v, phi1_v, i_v, w_v, pi_v, cw_v, cr_v,
                                      un_v, v_v, delta_v, r_b_v, r_z_v, i_b_v, i_z_v,
                                      K_v = 100000) {
  
  g <- phi0_v - phi1_v*i_v
  Yfc_v <- K_v/((1+g)*v_v)
  Y_v   <- un_v * Yfc_v
  W_v   <- w_v * Y_v
  h_v   <- (g+delta_v)*v_v/un_v
  I_v   <- h_v * Y_v
  
  A       <- W_v * (1 - beta_v*(r_b_v+i_b_v)/(1+g))
  Bcoef   <- (r_z_v+i_z_v)/(g+r_z_v)
  Cconst  <- i_b_v*beta_v*W_v/(1+g)
  Dcoef   <- i_z_v/(g+r_z_v)
  Bw_v    <- beta_v*W_v*(r_b_v+g)/(1+g)
  
  numerador   <- Y_v*(1-cr_v*pi_v) - cw_v*A - Bw_v - I_v - cr_v*Cconst
  denominador <- 1 - cw_v*Bcoef + cr_v*Dcoef
  
  Zw_v <- numerador/denominador
  lz_y <- (1+g)/(g+r_z_v) * (Zw_v/Y_v)
  
  list(g = g, Zw_Y = Zw_v/Y_v, lz_y = lz_y, Zw_v = Zw_v, Y_v = Y_v, denominador = denominador)
}
