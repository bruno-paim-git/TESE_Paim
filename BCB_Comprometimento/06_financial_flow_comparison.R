
date_file <- "2026-04-10" 

ff_final <- readr::read_rds(file = paste0("./Output/ff_final_", date_file, ".rds"))
dados_macro <-  readr::read_rds(file = paste0("./Output/dados_macro_",  date_file, ".rds"))

ff_bcb <- readxl::read_xlsx(".\\Input\\rpm202512anp.xlsx", sheet = "Graf 1.2.22", skip = 8) |>
  select(`Mês`, `Pessoas físicas`) |> 
  rename(date = `Mês`,
         ff_bcb_rpm = `Pessoas físicas`) |> 
  mutate(date = as.Date(date))

#R$ bilhões de out 2025, a.s., MM3M

chapter_section <- "chapter_3_2\\"



dados_macro <- dados_macro |> 
  select(date, IPCA)

ff_estimativa <- ff_final |> 
  group_by(date) |> 
  summarise(ff_bcb = sum(ff_bcb),
            ff_bru = sum(ff_bru))

precos_de <- "2025-10-01"

dessaz_safe <- function(x, start = c(2015, 01), freq = 12) {
  
  # remove NA / NaN para checar se sobra algo útil
  x_ok <- x[is.finite(x)]
  
  # se não há dados válidos ou se a série é constante
  if (length(x_ok) < freq || sd(x_ok) == 0) {
    return(x)
  }
  
  tryCatch(
    final(seas(ts(x, start = start, frequency = freq))),
    error = function(e) x
  )
}

colnames(ff_estimativa)

ff_bru <- ff_estimativa |> 
  left_join(dados_macro, by = join_by(date)) |> 
  as_tsibble(index = date) |> 
  arrange(date) %>%
  mutate(ff_bcb = ff_bcb*IPCA[which(date==precos_de)]/IPCA,
         ff_bru = ff_bru*IPCA[which(date==precos_de)]/IPCA) |> 
  select(-IPCA) |> drop_na() |> 
  mutate(
    across(
      where(is.numeric),
      dessaz_safe,
      .names = "sa_{.col}"
    )) |> 
  mutate(
    mm_sa_ff_bcb = rollmean(sa_ff_bcb , k = 3, fill = NA, align = "right")/1000,
    mm_sa_ff_bru = rollmean(sa_ff_bru , k = 3, fill = NA, align = "right")/1000,
  )


ff_df <- ff_bru |> left_join(ff_bcb) |> drop_na() |> select(date, mm_sa_ff_bcb, mm_sa_ff_bru, ff_bcb_rpm)




ff_df_long <- ff_df |> 
  pivot_longer(cols = c(mm_sa_ff_bcb, mm_sa_ff_bru, ff_bcb_rpm),
               names_to = "origem",
               values_to = "valor")

# 2. Plotar o gráfico
ggplot(ff_df_long ,
       aes(x = date, y = valor, color = origem)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept  = 0)+
  theme_minimal() +
  labs(
    title = "Evolução de Total ao Longo do Tempo",
    x = "Data",
    y = "Total",
    color = "Tipo"
  ) 

cor(ff_df$mm_sa_ff_bcb, 
    ff_df$ff_bcb_rpm, use = "complete.obs")



cor(ff_df$mm_sa_ff_bru, 
    ff_df$mm_sa_ff_bcb, use = "complete.obs")




######GRÁFICOS DISPERSÃO#####
#TOTAL
# Calcular limites do eixo para a linha 45º
lim_min <- min(ff_df$mm_sa_ff_bcb,
               ff_df$ff_bcb_rpm, na.rm = TRUE)
lim_max <- max(ff_df$mm_sa_ff_bcb,
               ff_df$ff_bcb_rpm, na.rm = TRUE)

# Criar gráfico com linha 45º
dispersão_total <- plot_ly(ff_df, 
                           x = ~mm_sa_ff_bcb, 
                           y = ~ff_bcb_rpm, 
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



ff_comparison_df <- ff_df |> select(-mm_sa_ff_bru) |> mutate(dif_pp = (ff_bcb_rpm - mm_sa_ff_bcb))

fluxo_financeiro_comparing <-  plot_ly(ff_comparison_df, x = ~date) %>%
  add_bars(y = ~dif_pp, name = "Diferença (p.p.)",  
           marker = list(color = "grey", opacity = 0.5), yaxis = "y2") %>%
  add_lines(y = ~ff_bcb_rpm, name = "Fluxo Financeiro (BCB)", 
            line = list(color = "red"), yaxis = "y") %>%
  add_lines(y = ~mm_sa_ff_bcb, name = "Elaboração Própria", 
            line = list(color = "green"), yaxis = "y") %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "Fluxo Financeiro (R$ bilhões)", font = list(size = 20)), range = c(-50, 50)), # Ajusta para começar do zero
    yaxis2 = list(title = list(text = "Diferença (R$ bilhões)", font = list(size = 20)), range = c(-10, 10),
                  overlaying = "y",
                  side = "right",
                  showgrid = FALSE) ,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    margin = list(r = 50)  # Aumenta a margem direita# Centraliza a legenda abaixo
  )

 # 1. Salvar o gráfico como HTML temporário
 htmlwidgets::saveWidget(fluxo_financeiro_comparing, "temp_plot.html")
 
 # 2. Capturar imagem a partir do HTML
 webshot("temp_plot.html", file = paste0(".\\Output\\Plots\\", chapter_section, "fluxo_financeiro_comparing.png"), vwidth = 1600, vheight = 800)
 
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


df_fluxo_financeiro_sel <- ff_final |> filter(modalidade %in% modalidades_validas)



ff_estimativa_sel <- df_fluxo_financeiro_sel |> 
  group_by(date) |> 
  summarise(ff_bcb = sum(ff_bcb),
            ff_bru = sum(ff_bru))

precos_de <- "2025-10-01"




ff_bru_sel <- df_fluxo_financeiro_sel |> 
  left_join(dados_macro, by = join_by(date)) |> 
  # as_tsibble(index = date) |> 
  # arrange(date) %>%
  drop_na() |> 
  arrange(modalidade, date) |>
  ungroup() |> 
  mutate(ff_bcb = ff_bcb*IPCA[which(date==precos_de)]/IPCA,
         ff_bru = ff_bru*IPCA[which(date==precos_de)]/IPCA) |> 
  select(-IPCA) |> 
  mutate(
    across(
      where(is.numeric),
      dessaz_safe,
      .names = "sa_{.col}"
    )) |> 
  mutate(
    mm_sa_ff_bcb = rollmean(sa_ff_bcb , k = 3, fill = NA, align = "right"),
    mm_sa_ff_bru = rollmean(sa_ff_bru , k = 3, fill = NA, align = "right"),
  )


ff_df_sel <- ff_bru_sel |> select(date, modalidade,mm_sa_ff_bcb, mm_sa_ff_bru)




ff_df_long_sel <- ff_df_sel |> 
  pivot_longer(cols = c(mm_sa_ff_bcb, mm_sa_ff_bru),
               names_to = "origem",
               values_to = "valor")

# 2. Plotar o gráfico
ggplot(ff_df_long_sel |> filter(date >= "2016-02-01"),
       aes(x = date, y = valor, color = origem)) +
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


ff_df_long_sel <- ff_estimativa_sel |> 
  pivot_longer(cols = c(ff_bcb, ff_bru),
               names_to = "origem",
               values_to = "valor")

# 2. Plotar o gráfico
ggplot(ff_df_long_sel |> filter(date >= "2016-02-01"),
       aes(x = date, y = valor, color = origem)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept  = 0)+
  theme_minimal() +
  labs(
    title = "Evolução de Total ao Longo do Tempo",
    x = "Data",
    y = "Total",
    color = "Tipo"
  )

cor(ff_estimativa_sel$ff_bcb, 
    ff_estimativa_sel$ff_bru, use = "complete.obs")

