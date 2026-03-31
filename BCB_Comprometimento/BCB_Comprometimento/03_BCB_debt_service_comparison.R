
date_file <- "2026-03-29" 

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

modalidades_selecionadas <- c(
  "Aquisição de outros bens",
  "Aquisição de veículos",
  "Arrendamento mercantil de outros bens",
  "Arrendamento mercantil de veículos",
  "Cartão de crédito parcelado sem juros",
  "Cartão de crédito parcelado",
  "Cartão de crédito rotativo",
  "Cheque especial",
  "Crédito pessoal consignado para aposentados e pensionistas do INSS",
  "Crédito pessoal consignado para trabalhadores do setor privado",
  "Crédito pessoal consignado para trabalhadores do setor público", 
  "Crédito pessoal não consignado",
  "Crédito pessoal não consignado vinculado à composição de dívidas",
  "Financiamento imobiliário com taxas de mercado",
  "Financiamento imobiliário com taxas reguladas"
)

#Para agregar as variáveis semelhantes:
#no cálculo do ICC ponderamos pelo saldo da carteira
#no cálculo dos juros ponderamos pelas concessões, diferente do BCB que pondera também pelo saldo
#optamos desta forma, pois é justamente por causa do efeito diferente sobre o ICC e sobre os Juros
#que as concessões gradualmente impactam no saldo

df_empilhado <- df_empilhado |>
  filter(modalidade %in% modalidades_selecionadas)

####CÁLCULO COMPROMETIMENTO####
#####CÁLCULO PRÓPRIO#####
COMPROMETIMENTO <- df_empilhado |> 
  ungroup() |> 
  group_by(date) |> 
  summarise(AMORTIZACAO = sum(AMORTIZACAO, na.rm = TRUE),
            JUROS = sum(JUROS, na.rm = TRUE),
            PMT = sum(PMT, na.rm = TRUE)) |> 
  mutate(AMORTIZACAO = rollmean(AMORTIZACAO, k = 3, fill = NA, align = "right"),
         JUROS = rollmean(JUROS, k = 3, fill = NA, align = "right"),
         PMT = rollmean(PMT, k = 3, fill = NA, align = "right")) |>
  left_join(dados_macro |> select(date, RNDBF), by = join_by(date)) |> 
  mutate(comprometimento_total = (PMT/RNDBF)*100,
         comprometimento_amortizacao = (AMORTIZACAO/RNDBF)*100,
         comprometimento_juros = (JUROS/RNDBF)*100) |> 
  select(date, comprometimento_amortizacao, comprometimento_juros,comprometimento_total ) |> 
  filter(date >= "2013-03-01")

#####SÉRIE DO BCB#####


#####CONSOLIDA PARA COMPARAÇÃO####

df_comprometimento_BCB <- COMPROMETIMENTO |>  left_join(df_BCB_comprometimento, by = "date")

#29266 - Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional exceto crédito habitacional - Sem ajuste sazonal (RNDBF) - %

COMPROMETIMENTO_BCB <- df_comprometimento_BCB |> 
  drop_na() |> 
  mutate(dif_total = BCB_comprometimento_total - comprometimento_total,
         dif_amortizacao = BCB_comprometimento_amortizacao - comprometimento_amortizacao,
         dif_juros = BCB_comprometimento_juros - comprometimento_juros)



#####CORRELAÇÃO ENTRE SÉRIE DO BCB E CALCULADA#####
cor(COMPROMETIMENTO_BCB$comprometimento_amortizacao, 
    COMPROMETIMENTO_BCB$BCB_comprometimento_amortizacao, use = "complete.obs")
cor(COMPROMETIMENTO_BCB$comprometimento_juros, 
    COMPROMETIMENTO_BCB$BCB_comprometimento_juros, use = "complete.obs")
cor(COMPROMETIMENTO_BCB$comprometimento_total, 
    COMPROMETIMENTO_BCB$BCB_comprometimento_total, use = "complete.obs")

#####GRÁFICOS COMPARAÇÃO####
######GRÁFICO SÉRIES#####
#TOTAL
comprometimento_total <- plot_ly(COMPROMETIMENTO_BCB, x = ~date) %>%
  add_bars(y = ~dif_total, name = "Diferença (p.p.)",  
           marker = list(color = "grey", opacity = 0.5), yaxis = "y2") %>%
  add_lines(y = ~BCB_comprometimento_total, name = "BCB (29265)", 
            line = list(color = "red"), yaxis = "y") %>%
  add_lines(y = ~comprometimento_total, name = "Elaboração própria", 
            line = list(color = "green"), yaxis = "y") %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "Comprometimento", font = list(size = 20)), range = c(0, 50)), # Ajusta para começar do zero
    yaxis2 = list(title = list(text = "Diferença (p.p)", font = list(size = 20)),
                  overlaying = "y",
                  side = "right",
                  showgrid = FALSE) ,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    margin = list(r = 50)  # Aumenta a margem direita# Centraliza a legenda abaixo
  )

comprometimento_total

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(comprometimento_total, "temp_plot.html", selfcontained = TRUE)

webshot("temp_plot.html", file = paste0(".\\Output\\Plots\\", chapter_section, "comprometimento_total.png"), vwidth = 1600, vheight = 800)

#JUROS
comprometimento_juros <- plot_ly(COMPROMETIMENTO_BCB, x = ~date) %>%
  add_bars(y = ~dif_juros, name = "Diferença (p.p.)",  
           marker = list(color = "grey", opacity = 0.5), yaxis = "y2") %>%
  add_lines(y = ~BCB_comprometimento_juros, name = "BCB (29264)", 
            line = list(color = "red"), yaxis = "y") %>%
  add_lines(y = ~comprometimento_juros, name = "Elaboração própria", 
            line = list(color = "green"), yaxis = "y") %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "Comprometimento", font = list(size = 20)), range = c(0, 30)), # Ajusta para começar do zero
    yaxis2 = list(title = list(text = "Diferença (p.p)", font = list(size = 20)),
                  overlaying = "y",
                  side = "right",
                  showgrid = FALSE) ,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    margin = list(r = 50)  # Aumenta a margem direita# Centraliza a legenda abaixo
  )

comprometimento_juros

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(comprometimento_juros, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\Plots\\", chapter_section, "comprometimento_juros.png"), vwidth = 1600, vheight = 800)

#AMORTIZAÇÃO
comprometimento_amortizacao <- plot_ly(COMPROMETIMENTO_BCB, x = ~date) %>%
  add_bars(y = ~dif_amortizacao, name = "Diferença (p.p.)",  
           marker = list(color = "grey", opacity = 0.5), yaxis = "y2") %>%
  add_lines(y = ~BCB_comprometimento_amortizacao, name = "BCB (29263)", 
            line = list(color = "red"), yaxis = "y") %>%
  add_lines(y = ~comprometimento_amortizacao, name = "Elaboração Própria", 
            line = list(color = "green"), yaxis = "y") %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "Comprometimento", font = list(size = 20)), range = c(0, 30)), # Ajusta para começar do zero
    yaxis2 = list(title = list(text = "Diferença (p.p)", font = list(size = 20)),
                  overlaying = "y",
                  side = "right",
                  showgrid = FALSE) ,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    margin = list(r = 50)  # Aumenta a margem direita# Centraliza a legenda abaixo
  )

comprometimento_amortizacao

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(comprometimento_amortizacao, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\Plots\\", chapter_section, "comprometimento_amortizacao.png"), vwidth = 1600, vheight = 800)

######GRÁFICOS DISPERSÃO#####
#TOTAL
# Calcular limites do eixo para a linha 45º
lim_min <- min(COMPROMETIMENTO_BCB$BCB_comprometimento_total,
               COMPROMETIMENTO_BCB$comprometimento_total, na.rm = TRUE)
lim_max <- max(COMPROMETIMENTO_BCB$BCB_comprometimento_total,
               COMPROMETIMENTO_BCB$comprometimento_total, na.rm = TRUE)

# Criar gráfico com linha 45º
dispersão_total <- plot_ly(COMPROMETIMENTO_BCB, 
                           x = ~BCB_comprometimento_total, 
                           y = ~comprometimento_total, 
                           type = "scatter", mode = "markers",
                           marker = list(color = "blue", opacity = 0.6, size = 15),
                           name = "Comprometimento da Renda com Serviço da Dívida") %>%
  add_trace(
    x = c(lim_min, lim_max),
    y = c(lim_min, lim_max),
    type = "scatter",
    mode = "lines",
    line = list(dash = "solid", color = "black", width = 2),
    name = "Linha 45°",
    marker = NULL
  ) %>%
  layout(
    xaxis = list(title = "BCB", titlefont = list(size = 20), tickfont = list(size = 16)),
    yaxis = list(title = "Elaboração Própria", titlefont = list(size = 20), tickfont = list(size = 16)),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18))
  )

dispersão_total

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(dispersão_total, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\Plots\\", chapter_section, "dispersão_total.png"), vwidth = 1600, vheight = 800)


#JUROS
plot_ly(COMPROMETIMENTO_BCB, x = ~BCB_comprometimento_juros, y = ~comprometimento_juros, type = "scatter", mode = "markers",
        marker = list(color = "blue", opacity = 0.6)) %>%
  layout(title = "Dispersão Comprometimento - JUROS",
         xaxis = list(title = "BCB"),
         yaxis = list(title = "Calculado"))
#AMORTIZAÇÃO
plot_ly(COMPROMETIMENTO_BCB, x = ~BCB_comprometimento_amortizacao, y = ~comprometimento_amortizacao, type = "scatter", mode = "markers",
        marker = list(color = "blue", opacity = 0.6)) %>%
  layout(title = "Dispersão Comprometimento - AMORTIZAÇÃO",
         xaxis = list(title = "BCB"),
         yaxis = list(title = "Calculado")) 

