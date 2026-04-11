


date_file <- "2026-04-10"

df_empilhado <- readr::read_rds(file = paste0("./Output/df_empilhado_", date_file, ".rds"))

chapter_section <- "chapter_3_2\\"


# colnames(df_empilhado)

df_fluxo_financeiro <- df_empilhado |>
  arrange(modalidade, date) |>
  filter(date >= "2013-01-01") |>
  mutate(JUROS = coalesce(JUROS, 0),
         AMORTIZACAO = coalesce(AMORTIZACAO, 0),
         var_saldo_bcb = saldo_bcb - lag(saldo_bcb),
         var_saldo_bru = saldo_bru - lag(saldo_bru),
         inadimplencia = coalesce(inadimplencia, 0),
         bp = ifelse(
           date <= as.Date("2024-12-01"),
           (0.1*inadimplencia/100) * saldo,
           (0.05* inadimplencia/100) * saldo
         ),
         # bp = 0,
         pgto_bcb = concessoes_bcb - var_saldo_bcb + JUROS - bp ,
         # pgto_bcb = concessoes_bcb - var_saldo_bcb + JUROS,
         pgto_bru = AMORTIZACAO + JUROS,
         ff_bcb = concessoes_bcb - pgto_bcb,
         ff_bru = concessoes_bru - pgto_bru,
         ) |> 
  rename(concessoes_orig = concessoes)


ff_final <- df_fluxo_financeiro |>
  select(date, modalidade, ff_bcb, ff_bru, pgto_bcb, pgto_bru, concessoes_bcb, concessoes_orig, concessoes_bru) |> 
  filter(date >= as.Date("2015-01-01")) |>
  arrange(modalidade, date) |>
  group_by(modalidade) |>
  mutate(
    
    # -----------------------
    # Acumulado 12 meses
    # -----------------------
    across(
      where(is.numeric) & !c(date),
      ~ slide_dbl(.x, sum, .before = 11, .complete = TRUE),
      .names = "acum12m_{.col}"
    ),
    
    # -----------------------
    # Média móvel 3 meses
    # -----------------------
    across(
      where(is.numeric) & !c(date) &!starts_with("acum12m_"),
      ~ slide_dbl(.x, mean, .before = 2, .complete = TRUE),
      .names = "mm3m_{.col}"
    )
    
  ) |>
  ungroup()


#####save_the_data####


readr::write_excel_csv2(ff_final,
                        file = paste0("./Output/ff_final_",  date_file, ".csv"))


readr::write_rds(ff_final,
                 file = paste0("./Output/ff_final_",  date_file, ".rds"))

readr::write_rds(df_fluxo_financeiro,
                 file = paste0("./Output/df_fluxo_financeiro_",  date_file, ".rds"))


ff_long <- ff_final |> mutate(dif_ff = ff_bcb - ff_bru,
                              dif_pgto = (pgto_bcb - pgto_bru),
                              dif_concessoes = concessoes_bcb - concessoes_bru) |> 
  pivot_longer(cols = c(ff_bcb, ff_bru, 
                        pgto_bcb, pgto_bru,
                        concessoes_bcb, concessoes_orig, concessoes_bru,
                        dif_ff, dif_pgto, dif_concessoes),
               names_to = "tipo",
               values_to = "valor")



# 2. Plotar o gráfico
ggplot(ff_long |> filter(
  tipo == "dif_concessoes" |
    tipo == "dif_pgto"
),
aes(x = date, y = valor, color = tipo)) +
  geom_point(aes(shape = tipo), size = 1.2)+
  geom_line(linewidth = 1) +
  geom_hline(yintercept  = 0)+
  theme_minimal() +
  labs(
    title = "Evolução de Total ao Longo do Tempo",
    x = "Data",
    y = "Total",
    color = "Tipo"
  ) +
  facet_wrap(~ modalidade, scales = "free_y")



modalidades_validas <- c(
  "Aquisição de outros bens",
  "Aquisição de veículos",
  "Arrendamento mercantil de outros bens" ,
  "Arrendamento mercantil de veículos",
  "Cartão de crédito parcelado sem juros" ,
  "Cartão de crédito parcelado" ,
  "Cartão de crédito rotativo" ,
  "Cheque especial" ,
  "Crédito pessoal consignado para aposentados e pensionistas do INSS" ,
  "Crédito pessoal consignado para trabalhadores do setor privado",
  "Crédito pessoal consignado para trabalhadores do setor público",
  "Crédito pessoal não consignado vinculado à composição de dívidas",
  "Crédito pessoal não consignado",
  "Financiamento imobiliário com taxas de mercado",
  "Financiamento imobiliário com taxas reguladas"
)

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
  "Crédito pessoal não consignado vinculado à composição de dívidas" = "Crédito pessoal não consignado",
  "Crédito pessoal não consignado" = "Crédito pessoal não consignado",
  "Financiamento imobiliário com taxas de mercado" = "Imobiliário",
  "Financiamento imobiliário com taxas reguladas" = "Imobiliário"
)


####PRINCIPAL ARQUIVO####
ff_long_agregado <- ff_long |>
  mutate(modalidade_agregada = coalesce(modalidades_agregadas[modalidade], "Outros")) |> 
  group_by(date, tipo, modalidade_agregada) |> 
  summarise(
    across(
      where(is.numeric) & !any_of("date"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )



df_variables <- tibble::tibble(sel_variable = c(
"ff",
"concessoes",
"pgto"))

data_series_comparison <- function(sel_variable, data = ff_long_agregado){
  
  extense_selected_variable <- case_match(sel_variable,
                                          "ff"         ~ "Fluxo Financeiro",
                                          "concessoes" ~ "Concessões",
                                          "pgto"       ~ "Pagamento",
                                          .default     = NA_character_ # Optional: handles values that don't match
  )
  
  data_series_plot <- ggplot() +
    # 🔴 Linha da resultante
    geom_col(
      data = ff_long_agregado |> 
        filter(tipo == paste0("dif_",sel_variable)),
      aes(x = date, y = valor, fill = "Diferença")
      
    )+
    # 🔵 Barras
    geom_line(
      data = ff_long_agregado |> 
        filter(tipo == (paste0(sel_variable, "_bcb"))),
      aes(x = date, y = valor, color = "Abordagem Contábil"), linewidth = 0.8
    ) +
    # 🔵 Barras
    geom_line(
      data = ff_long_agregado |> 
        filter(tipo == (paste0(sel_variable, "_bru"))),
      aes(x = date, y = valor, color = "Abordagem Estoque"), linewidth = 0.8
    )   +
    
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
    scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
    scale_color_manual(
      values = c("Abordagem Contábil" = "blue3",
                 "Abordagem Estoque" = "red4"),  # Ordem desejada
      breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
    ) +
    scale_fill_manual(values = "darkgray") +
    labs(x = "Data",
         y = paste0(extense_selected_variable, " (R$ milhões)"),
         color = "Legenda") +  # Nome da legenda
    theme_minimal(base_size = 12) + 
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
      plot.margin = margin(10, 30, 10, 10) # Aumentei a margem direita (30)
    )+
    facet_wrap(~modalidade_agregada, ncol = 3, scales = "free_y")
  # data_series_plot
  
  message("Variável: ", extense_selected_variable)
  ggsave(
    filename = paste0(".\\Output\\Plots\\", chapter_section, "data_series_", sel_variable,".svg"),
    plot = data_series_plot, width = 16, height = 8, dpi = 1000)
}

pwalk(df_variables, data_series_comparison)

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
  "Crédito pessoal não consignado vinculado à composição de dívidas" = "Crédito pessoal não consignado",
  "Crédito pessoal não consignado" = "Crédito pessoal não consignado",
  "Financiamento imobiliário com taxas de mercado" = "Imobiliário",
  "Financiamento imobiliário com taxas reguladas" = "Imobiliário"
)


####PRINCIPAL ARQUIVO####
ff_long_agregado_2 <- ff_long |>
  mutate(modalidade_agregada = coalesce(modalidades_agregadas[modalidade], "Outros")) |> 
  group_by(date, tipo, modalidade_agregada) |> 
  summarise(
    across(
      where(is.numeric) & !any_of("date"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


data_series_comparison_nova_categoria <- function(sel_variable, data = ff_long_agregado_2){
  
  extense_selected_variable <- case_match(sel_variable,
                                          "ff"         ~ "Fluxo Financeiro",
                                          "concessoes" ~ "Concessões",
                                          "pgto"       ~ "Pagamento",
                                          .default     = NA_character_ # Optional: handles values that don't match
  )
  
  data_series_plot <- ggplot() +
    # 🔴 Linha da resultante
    geom_col(
      data = data |> 
        filter(tipo == paste0("dif_",sel_variable)),
      aes(x = date, y = valor, fill = "Diferença")
      
    )+
    # 🔵 Barras
    geom_line(
      data = data |> 
        filter(tipo == (paste0(sel_variable, "_bcb"))),
      aes(x = date, y = valor, color = "Abordagem Contábil"), linewidth = 0.8
    ) +
    # 🔵 Barras
    geom_line(
      data = data |> 
        filter(tipo == (paste0(sel_variable, "_bru"))),
      aes(x = date, y = valor, color = "Abordagem Estoque"), linewidth = 0.8
    )   +
    
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
    scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
    scale_color_manual(
      values = c("Abordagem Contábil" = "blue3",
                 "Abordagem Estoque" = "red4"),  # Ordem desejada
      breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
    ) +
    scale_fill_manual(values = "darkgray") +
    labs(x = "Data",
         y = paste0(extense_selected_variable, " (R$ milhões)"),
         color = "Legenda") +  # Nome da legenda
    theme_minimal(base_size = 12) + 
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
      plot.margin = margin(10, 30, 10, 10) # Aumentei a margem direita (30)
    )+
    facet_wrap(~modalidade_agregada, ncol = 3, scales = "free_y")
  # data_series_plot
  
  message("Variável: ", extense_selected_variable)
  ggsave(
    filename = paste0(".\\Output\\Plots\\", chapter_section, "data_series_agregated_", sel_variable,".svg"),
    plot = data_series_plot, width = 16, height = 8, dpi = 1000)
}

pwalk(df_variables, data_series_comparison_nova_categoria)

