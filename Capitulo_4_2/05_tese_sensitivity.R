
####MULTI_SHOCK####
shock_multi <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.0918,
  ),
  start = 50,
  end = 1000
)

#### Choque em i####
CHOQUE <- sort(c(seq(0.08, 0.09, 0.01), 0.0818)) #CHOQUE i
schock_exp <- sfcr_expand(shock_multi, i, CHOQUE)
tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA135_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_135 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA135_PHI06",
         valor_beta = "0.135")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA5_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_5 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA5_PHI06",
         valor_beta = "0.5")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA0_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_0 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA0_PHI06",
         valor_beta = "0")

df_scenario1_TOTAL <- rbind(df_scenario1_TOTAL_135, df_scenario1_TOTAL_5)%>%
  mutate(MODELO_BASE = factor(MODELO_BASE))

df_scenario1_TOTAL <- rbind(df_scenario1_TOTAL_135, df_scenario1_TOTAL_5,df_scenario1_TOTAL_0)%>%
  mutate(MODELO_BASE = factor(MODELO_BASE))


# Criação do gráfico

plot_sensibility_beta <- df_scenario1_TOTAL %>%
  filter(name == "u", period < 400) %>%
  ggplot(aes(
    x = period,
    y = value,
    color = CHOQUE,
    linetype = factor(valor_beta),  # Tipo de linha → valor de beta
  )) +
  geom_line(linewidth = 1) +
  
  # Escalas de cores e linhas
  scale_color_brewer(
    palette = "Set1", 
    name = "Choque na Taxa de Juros (i)",
    labels = as.character(CHOQUE) # Ajuste os valores conforme seus dados  ) +
  )+
  scale_linetype_manual(
    name = "Valor de Beta", 
    values = c("solid", "dotted","dashed")
  ) +
  
  # Limites dos eixos
  scale_y_continuous(
    limits = c(0.7, 0.85),
    expand = c(0, 0)
  ) +
  
  # Títulos dos eixos
  labs(
    x = "Períodos",
    y = "Utilização da Capacidade Instalada (u)"  ) +
  
  # Tema
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.position = "bottom"
  )

plot_sensibility_beta

ggsave(filename = paste0(".\\Output\\sensitivity_beta_i.svg"), 
       plot = plot_sensibility_beta, width = 16, height = 8, dpi = 1000)


# Choque em phi0
shock_multi <- sfcr_shock(
  variables = sfcr_set(
    phi0 ~ 0.06,
  ),
  start = 50,
  end = 1000
)
CHOQUE <- seq(0.05, 0.07, 0.01) #phi0
schock_exp <- sfcr_expand(shock_multi, phi0, CHOQUE)
tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA135_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_135 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA135_PHI06",
         valor_beta = "0.135")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA5_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_5 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA5_PHI06",
         valor_beta = "0.5")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA0_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_0 <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA0_PHI06",
         valor_beta = "0")


df_scenario1_TOTAL <- rbind(df_scenario1_TOTAL_135, df_scenario1_TOTAL_5)%>%
  mutate(MODELO_BASE = factor(MODELO_BASE))

# Criação do gráfico
plot_sensibility_beta_phi0 <- df_scenario1_TOTAL %>%
  filter(name == "u", period < 400) %>%
  ggplot(aes(
    x = period,
    y = value,
    color = CHOQUE,
    linetype = factor(valor_beta)  # Tipo de linha → valor de beta
  )) +
  geom_line(linewidth = 1) +
  
  # Escalas de cores e linhas
  scale_color_brewer(
    palette = "Set1", 
    name = "Choque em (phi_0)",
    labels = as.character(CHOQUE)
    )+#
  scale_linetype_manual(
    name = "Valor de beta", 
    values = c("solid", "dotted", "dashed", "dotdash", "longdash", "twodash")
  ) +
  
  # Limites dos eixos
  scale_y_continuous(
    limits = c(0.5, 1),
    expand = c(0, 0)
  ) +
  
  # Títulos dos eixos
  labs(
    x = "Períodos",
    y = "Utilização da Capacidade Instalada (u)"  ) +
  
  # Tema
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.position = "bottom"
  )
plot_sensibility_beta_phi0
ggsave(filename = paste0(".\\Output\\plot_sensibility_beta_phi0.png"), plot = plot_sensibility_beta_phi0, width = 16, height = 8, dpi = 800)






# GRÁFICO u
df_scenario1_TOTAL |> 
  filter(name %in% c("u") ) |> 
  filter( period <400  ) |> 
  plot_ly(
    x = ~period,
    y = ~value,
    color = CHOQUE,  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "Utilização da Capacidade Instalada (u)",
                 range = c(0.7, 0.85) ),  # Set the y-axis label
    legend = list(title = list(text = "Choques na Taxa de Juros (i)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
    # legend = list(title = list(text = "Choques no componente autônomo (phi0)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
    
  ) 


# GRÁFICO h
df_scenario1_TOTAL |> 
  filter(name %in% c("h") ) |> 
  filter( period <800  ) |> 
  plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(MODELO_BASE, labels = as.character(unique(df_scenario1_TOTAL$CHOQUE)),  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "Propensão Marginal a Investir (h)"),  # Set the y-axis label
    legend = list(title = list(text = "Choques na Taxa de Juros (i)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )

# GRÁFICO dsw_y
df_scenario1_TOTAL |> 
  filter(name %in% c("dsw_y") ) |> 
  filter( period <800  ) |> 
  plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(MODELO_BASE, labels = as.character(CHOQUE)),  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "Serviço da Dívida em relação aos Salários"),  # Set the y-axis label
    legend = list(title = list(text = "Choques na Taxa de Juros (i)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )

# GRÁFICO dsw_y
df_scenario1_TOTAL |> 
  filter(name %in% c("Ydw") ) |> 
  filter( period <800  ) |> 
  plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(MODELO_BASE, labels = as.character(CHOQUE)),  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "Serviço da Dívida em relação aos Salários"),  # Set the y-axis label
    legend = list(title = list(text = "Choques na Taxa de Juros (i)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )


# GRÁFICO logY
df_scenario1_TOTAL |> 
  filter(name %in% c("log_y") ) |> 
  filter( period <800  ) |> 
  plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(MODELO_BASE, labels = as.character(CHOQUE)),  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "Serviço da Dívida em relação aos Salários"),  # Set the y-axis label
    legend = list(title = list(text = "Choques na Taxa de Juros (i)"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )
