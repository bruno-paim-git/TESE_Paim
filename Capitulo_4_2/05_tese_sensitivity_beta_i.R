
####MULTI_SHOCK####
shock_multi <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.04,
  ),
  start = 10,
  end = 1000
)

#### Choque em i####
CHOQUE <- sort(c(0.05, 0.04, 0.06)) #CHOQUE i


schock_exp <- sfcr_expand(shock_multi, i, CHOQUE)
tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA03_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_A <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA03_PHI06",
         valor_beta = "0.03")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA06_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_B <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA06_PHI06",
         valor_beta = "0.06")

tese_multi <- sfcr_multis(schock_exp, lista_modelos_baseline[["BETA00_PHI06"]], periods = 1000, method = "Newton")
names(tese_multi) <- CHOQUE
df_scenario1_TOTAL_C <- map_df(tese_multi, process_model_df, .id = "CHOQUE")|>
  mutate(MODELO_BASE = "BETA00_PHI06",
         valor_beta = "0")

df_scenario1_TOTAL <- rbind(df_scenario1_TOTAL_A, df_scenario1_TOTAL_B,df_scenario1_TOTAL_C)%>%
  mutate(MODELO_BASE = factor(MODELO_BASE))
# df_scenario1_TOTAL <- rbind(df_scenario1_TOTAL_A, df_scenario1_TOTAL_B)%>%
#   mutate(MODELO_BASE = factor(MODELO_BASE))

# Criação do gráfico

plot_sensibility_beta <- df_scenario1_TOTAL %>%
  filter(name == "u", period < 100) %>%
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
  # 
  # # Limites dos eixos
  # scale_y_continuous(
  #   limits = c(0.7, 0.9),
  #   expand = c(0, 0)
  # ) +
  # 
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


