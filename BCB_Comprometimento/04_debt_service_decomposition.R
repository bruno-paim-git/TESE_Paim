date_file <- "2026-04-10" 

df_empilhado <- readr::read_rds(file = paste0("./Output/df_empilhado_", date_file, ".rds"))
dados_macro <-  readr::read_rds(file = paste0("./Output/dados_macro_",  date_file, ".rds"))
df_BCB_comprometimento <-  readr::read_rds(file = paste0("./Output/df_BCB_comprometimento_",  date_file, ".rds"))


chapter_section <- "chapter_3_2\\"

#Agrega Modalidades
modalidades_agregadas <- c(
  "Aquisição de outros bens" = "Aquisição de bens",
  "Aquisição de veículos" = "Aquisição de bens",
  "Arrendamento mercantil de outros bens" = "Aquisição de bens",
  "Arrendamento mercantil de veículos" = "Aquisição de bens",
  "Cartão de crédito parcelado sem juros"  = "CC parcelado sem juros",
  "Cartão de crédito parcelado"  = "CC parcelado",
  "Cartão de crédito rotativo"  = "CC rotativo",
  "Cheque especial" = "Cheque especial",
  "Crédito pessoal consignado para aposentados e pensionistas do INSS" = "Consignado",
  "Crédito pessoal consignado para trabalhadores do setor privado" = "Consignado",
  "Crédito pessoal consignado para trabalhadores do setor público" = "Consignado",
  "Crédito pessoal não consignado" = "Crédito pessoal não consignado",
  "Crédito pessoal não consignado vinculado à composição de dívidas" = "Crédito pessoal não consignado",
  "Financiamento imobiliário com taxas de mercado" = "Imobiliário",
  "Financiamento imobiliário com taxas reguladas" = "Imobiliário"
)


####PRINCIPAL ARQUIVO####
total <- df_empilhado |>
  mutate(modalidade_agregada = coalesce(modalidades_agregadas[modalidade], "Outros")) |> 
  group_by(date, modalidade_agregada) |> 
  summarise(SALDO = sum(saldo_bru),
            AMORTIZACAO = sum(AMORTIZACAO),
            JUROS = sum(JUROS),
            PMT = sum(PMT),
            CONCESSOES = sum(concessoes_bru),
            FLUXO_FINANCEIRO = sum(CONCESSOES) - sum(PMT),
            ICC_AM = sum(icc_am*saldo_bru)/sum(saldo_bru),
            TX_JUROS = sum(tx_juros*concessoes_bru)/sum(concessoes_bru)) |> 
  filter(SALDO > 0)



comp_modalidade <- list()

modalidade_modelo <- c(          
  "Aquisição de bens",
  "CC parcelado sem juros",
  "CC parcelado",
  "CC rotativo",
  "Cheque especial",
  "Consignado",
  "Crédito pessoal não consignado",
  "Imobiliário")

cores_modalidades <- c(
  "Aquisição de bens" = "#1f77b4",  # Azul
  "CC parcelado sem juros"= "#7f7f7f", #Cinza
  "CC parcelado" = "#ff7f0e",    # Laranja
  "CC rotativo" = "#2ca02c",  # Verde
  "Cheque especial" = "#d62728",     # Vermelho
  "Consignado" = "#bcbd22",  # Roxo
  "Crédito pessoal não consignado" = "#8c564b",  # Marrom
  "Imobiliário" = "#e377c2"   # Rosa
)

for (modalidade_selecionada in modalidade_modelo) {
  
  print(modalidade_selecionada)
  
  df_data <- total |> 
    filter(modalidade_agregada ==
             modalidade_selecionada
    ) |>
    left_join(dados_macro, by = join_by(date)) |> 
    as_tsibble(index = date) |>  # <- Define 'date' como índice corretamente
    dplyr::select(-modalidade_agregada) %>%
    ungroup() |> 
    mutate(amort_liq = CONCESSOES-AMORTIZACAO) |> 
    #cria variáveis com a taxa de crescimento
    mutate(mm_3m_FLUXO_FINANCEIRO = rollmean(FLUXO_FINANCEIRO, k = 3, fill = NA, align = "right"),
           mm_3m_AMORTIZACAO = rollmean(AMORTIZACAO, k = 3, fill = NA, align = "right"),
           mm_3m_JUROS = rollmean(JUROS, k = 3, fill = NA, align = "right"),
           mm_3m_PMT = rollmean(PMT, k = 3, fill = NA, align = "right")) |> 
    #cria variáveis de impulso 
    mutate(impulso_pmt = -(rollmean(PMT, k = 3, fill = NA, align = "right") - lag(rollmean(PMT, k = 3, fill = NA, align = "right"), 12)),
           impulso_con = rollmean(CONCESSOES, k = 3, fill = NA, align = "right") - lag(rollmean(CONCESSOES, k = 3, fill = NA, align = "right"), 12),
           impulso_amort = rollmean(amort_liq, k = 3, fill = NA, align = "right") - lag(rollmean(amort_liq, k = 3, fill = NA, align = "right"), 12),
           impulso_juros = -(rollmean(JUROS, k = 3, fill = NA, align = "right") - lag(rollmean(JUROS, k = 3, fill = NA, align = "right"), 12)),
           impulso = rollmean(FLUXO_FINANCEIRO, k = 3, fill = NA, align = "right") - lag(rollmean(FLUXO_FINANCEIRO, k = 3, fill = NA, align = "right"), 12)) |> 
    #SEM AJUSTE
    mutate(comprometimento_total = (mm_3m_PMT/RNDBF)*100,
           comprometimento_amortizacao = (mm_3m_AMORTIZACAO/RNDBF)*100,
           comprometimento_juros = (mm_3m_JUROS/RNDBF)*100) |> 
    #COM AJUSTE SAZONAL
    mutate(comprometimento_total_saz = (mm_3m_PMT/RNDBF_ajustado)*100,
           comprometimento_amortizacao_saz = (mm_3m_AMORTIZACAO/RNDBF_ajustado)*100,
           comprometimento_juros_saz = (mm_3m_JUROS/RNDBF_ajustado)*100) |> 
    mutate(         var_FLUXO_FINANCEIRO = FLUXO_FINANCEIRO-lag(FLUXO_FINANCEIRO,12),
                    cresc_CONCESSOES = CONCESSOES/lag(CONCESSOES,12)-1
    )
  
  
  resultados <- df_data 
  
  comp_modalidade[[modalidade_selecionada]] <- resultados
  
}

comp_modalidade_total <- map2_df(comp_modalidade, names(comp_modalidade), ~ mutate(as_tibble(.x), origem = .y))

comp_modalidade_total <- comp_modalidade_total %>%
  group_by(date) %>%
  mutate(PMT_perc = PMT / sum(PMT),
         SALDO_perc = SALDO/sum(SALDO),
         PMT_TOTAL = sum(PMT),
         SALDO_TOTAL = sum(SALDO),
         JUROS_TOTAL = sum(JUROS, na.rm = TRUE),
         AMORTIZACAO_TOTAL = sum(AMORTIZACAO)) %>%
  ungroup()


write.csv2(comp_modalidade_total, file = ".\\Output\\comp_modalidade_total.csv")
####GRÁFICOS####
#####% DO TOTAL####

######GGPLOT####
# Criando o gráfico empilhado % do Total

####plot_decomp_comprometimento####
plot_decomp_comprometimento <- ggplot(comp_modalidade_total |> filter(date >= "2013-01-01"), 
                                      aes(x = date, y = PMT_perc*100, fill = origem)) +
  geom_col(position = "stack", width = 40) +  # Aumente o width para preencher o espaço
  labs(x = "Data",
       y = "% do Serviço da Dívida",
       fill = "Origem") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),                    # remove grades menores
    panel.grid.major.x = element_line(color = "grey80"),   # mantém apenas a grade principal em x
    panel.grid.major.y = element_line(color = "grey90")  
  ) +
  scale_fill_manual(values = cores_modalidades) +  # Atribui as cores às modalidades
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0))  # Remove espaços extras


plot_decomp_comprometimento

ggsave(filename = paste0(".\\Output\\Plots\\", chapter_section, "decomposicao_comprometimento.svg"), plot = plot_decomp_comprometimento, width = 16, height = 8, dpi = 800)

######PLOTLY####
plot_ly(
  data = comp_modalidade_total|> filter(date >= "2013-03-01"),
  x = ~date,
  y = ~PMT_perc,
  color = ~origem,
  colors = cores_modalidades,
  type = "bar",
  text = ~paste0(round(PMT_perc * 100, 1), "%"), # Adiciona rótulos de porcentagem
  textposition = "inside"
) %>%
  layout(
    title = "Participação no do total da PMT por modalidade - Brasil - 2013-2024",
    xaxis = list(title = "Data", tickformat = "%b %Y"), # Formata data no eixo X
    yaxis = list(title = "% PMT", tickformat = ".0%"), # Mostra eixo Y em porcentagem
    barmode = "stack",
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
  )

#####POR MODALIDADE ACUMULADO####
# Criando o gráfico empilhado
ggplot(comp_modalidade_total |> filter(date >= "2013-03-01"), 
       aes(x = date, y = comprometimento_total_saz, fill = origem)) +
  geom_col(position = "stack", width = 400) +  # Aumente o width para preencher o espaço
  labs(title = "Comprometimento da Renda por modalidade - Brasil - 2013-2024",
       x = "Data",
       y = "% da RNDBF",
       fill = "Origem") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = cores_modalidades) +  # Atribui as cores às modalidades
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = c(0,0))  # Remove espaços extras

#####COMPROMETIMENTO POR MODALIDADE####
######GGPLOT####

####plot_decomp_comp_linhas####

plot_decomp_comp_linhas <- ggplot(comp_modalidade_total|> filter(date >= "2013-03-01"), aes(x = date, y = round(comprometimento_total_saz, 2), color = origem)) +
  geom_line(size = 1.5) +  # Desenha as linhas conectando os pontos
  labs(x = "Data", 
       y = "% da RNDBF") +
  scale_x_date(labels = scales::date_format("%Y"), breaks = "1 year") +  # Formata a data no eixo X
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Formata o eixo Y em percentual
  theme_minimal() + 
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),                    # remove grades menores
    panel.grid.major.x = element_line(color = "grey80"),   # mantém apenas a grade principal em x
    panel.grid.major.y = element_line(color = "grey90")  
  ) +
  scale_color_manual(values = cores_modalidades) +  # Atribui as cores às modalidades
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0))

plot_decomp_comp_linhas

ggsave(filename = paste0(".\\Output\\Plots\\", chapter_section, "decomposicao_comprometimento_linhas.svg"), plot = plot_decomp_comp_linhas, width = 16, height = 8, dpi = 800)


######PLOTLY####

plot_ly(
  data = comp_modalidade_total |> filter(date >= "2013-03-01"),
  x = ~date,
  y = ~round(comprometimento_total_saz, 2),
  color = ~origem,
  colors = cores_modalidades,
  type = "scatter",
  mode = 'lines',
  textposition = "inside"
) %>%
  layout(
    title = "Comprometimento da Renda por modalidade - Brasil - 2013-2024",
    xaxis = list(title = "Data", tickformat = "%b %Y"), # Formata data no eixo X
    yaxis = list(title = "% da RNDBF", tickformat = "0.0 %"), # Mostra eixo Y em porcentagem
    barmode = "stack",
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
  )

#####SCATTER#####

####plot_perfil_2013_2024####

df_filtrado <- comp_modalidade_total |> 
  filter(date == "2013-12-01" | date == "2025-12-01") |> 
  arrange(origem, date) |> 
  group_by(origem) |> 
  mutate(
    xend = lead(PMT_perc * 100), 
    yend = lead(SALDO_perc * 100)
  ) |> 
  ungroup()

plot_perfil_2013_2025<- ggplot(df_filtrado, aes(x = PMT_perc * 100, y = SALDO_perc * 100, color = origem)) +
  geom_point(size = 10) +  # Pontos individuais
  geom_segment(
    aes(xend = xend, yend = yend),
    arrow = arrow(length = unit(0.4, "cm")),  # Adiciona seta para indicar direção
    color = "black", linewidth = 0.8,
    na.rm = TRUE  # Remove segmentos que não têm um segundo ponto
  ) +
  scale_color_manual(values = cores_modalidades) +  # Define cores das modalidades
  labs(
    x = "% do Serviço da Dívida",
    y = "% do Saldo",
    color = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.minor = element_blank(),                    # remove grades menores
    panel.grid.major.x = element_line(color = "grey80"),   # mantém apenas a grade principal em x
    panel.grid.major.y = element_line(color = "grey90")  
  )


plot_perfil_2013_2025

ggsave(filename = paste0(".\\Output\\Plots\\", chapter_section, "perfil_2013_2025.svg"), plot = plot_perfil_2013_2025, width = 16, height = 8, dpi = 800)
