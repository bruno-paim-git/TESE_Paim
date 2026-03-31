
####SCHOKS####

period_simulation <- 1000

####função para adicionar variáveis de análise nos modelos simulados
process_model_df <- function(df_model) {
  df_model |>
    mutate(
      #serviço da dívida
      #serviço consumo induzido
      dsbw_y = ((r_b+i*lambda_b)*Lbw)/(w*Y),                                     
      #serviço consumo autônomo
      dszw_y = ((r_z+i*lambda_z)*Lzw)/(w*Y),
      #serviço total
      dsw_y = dsbw_y + dszw_y,
      #consumo em relação ao estoque de capital
      cw_k= Cw/lag(K),
      cr_k= Cr/lag(K),
      #consumo em relação à renda
      cw_y= Cw/Y,
      cr_y= Cr/Y,
      bw_y = Bw/Y,
      zw_y = Zw/Y,
      #nível de consumo
      log_Cw = log(Cw),
      #dívida em relação aos salários
      lbw= Lbw/W,
      lzw= Lzw/W,
      lw= Lw/W,
      #nível da renda
      log_y = log(Y),
      log_ydw = log(Ydw),
      gY = Y/lag(Y) -1,
      #patrimônio Líquido
      Vw_y = Vw/Y,
      Vr_y = Vr/Y,
      rel_l = Lbw/Lzw,
      # SAVr_Y = SAVr/Y,
      Zw_L = Zw/Lw,
      # Zw líquido
      ZZw = Zw -(((r_b+i*lambda_b)*Lbw) + ((r_z+i*lambda_z)*Lzw)),
      gZZw = ZZw/lag(ZZw)-1,
      gW = W/lag(W)-1,
      dif_g = gW-gZZw
      # gCa = Ca/lag(Ca)-1,
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

gerar_subplot_ggplot <- function(vars) {
  
  df_plot <- df_scenario1_TOTAL %>% 
    dplyr::filter(name %in% vars, period <= 600)
  
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
salvar_subplot <- function(vars, filename) {
#plots ggplot
  plot_ggplot <- gerar_subplot_ggplot(vars)
  
  ggsave(filename = file.path("Output", paste0(filename, "_ggplot.png")), plot = plot_ggplot, 
         width = 12, height = 4, dpi = 800 )
  
  ggsave(filename = file.path("Output", paste0(filename, ".svg")), plot = plot_ggplot, 
         width = 12, height = 4, dpi = 800 )
  
  #plots plotly
  # plot <- gerar_subplot(vars)
  # # Salvar HTML temporário
  # temp_html <- tempfile(fileext = ".html")
  # htmlwidgets::saveWidget(plot, temp_html, selfcontained = TRUE)
  # 
  # # Salvar PNG na pasta Output
  # webshot(temp_html, file = file.path("Output", paste0(filename, "_plotly.png")), 
  #         vwidth = 1600, vheight = 600)

    }
