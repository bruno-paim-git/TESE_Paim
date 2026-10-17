# OBJETIVO:
# Consolida-se o cálculo de serviço da dívida seguindo as duas abordagens: Estoque e Contábil
# Calcula-se o Impulso de Crédito, dado pela variação interanual da média trimestral
# do Fluxo Financeiro (concessões líquidas), resultado da diferença entre as Concessões e o PMT.
# Exporta os gráficos comparativos entre as abordagens

#Os valores são deflacionados a preços recentes

#define data do arquivo base
date_file <- "2026-09-09" 
{
  ####BIBLIOTECA####
  library(tidyverse)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(vars)
  library(svars)
  library(TTR)
  library(tsibble)
  library(conflicted)
  library(plotly)
  library(seasonal)
  library(webshot)
  library(RCurl)
  library(slider)
  library(patchwork)
  library(tseries)
  library(urca)
  
  options(scipen = 999)
  
  conflict_prefer("layout", "plotly")
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  
  source("./function_export_plot.R")

}


df_empilhado <- readr::read_rds(file = paste0("./Output/df_empilhado_", date_file, ".rds"))
dados_macro <-  readr::read_rds(file = paste0("./Output/dados_macro_",  date_file, ".rds")) |> 
  select(-IBCBR, -RNDBF_ajustado, -SELIC, -PIB_acum12m)

chapter_section <- "chapter_3_2\\"

comp_modalidade_def <- list()

modalidade_modelo <- c(          
  "Aquisição de bens",
  "CC parcelado sem juros",
  "CC parcelado",
  "CC rotativo",
  "Cheque especial",
  "Consignado",
  "Crédito pessoal não consignado",
  "Imobiliário"
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

cores_modalidades <- c(
  "Aquisição de bens" = "#004488",  # Azul
  "CC parcelado sem juros" = "#DDAA33",    # Laranja
  "CC parcelado" = "#4477AA",                     # Azul Escuro Suave ou #EE6677 (Salmão)
  "CC rotativo" = "#666666",
  "Cheque especial" = "#BB5566",     # Vermelho
  "Consignado" = "#66CCEE",  # Roxo
  "Crédito pessoal não consignado" = "#228833",  # Marrom
  "Imobiliário" = "#AA3377"   # Rosa
)


df_fluxo_financeiro <- df_empilhado |>
  arrange(modalidade, date) |>
  group_by(modalidade) |> 
  filter(date >= "2013-01-01") |>
  mutate(JUROS = coalesce(JUROS, 0),
         AMORTIZACAO = coalesce(AMORTIZACAO, 0),
         var_saldo_bcb = saldo_bcb - lag(saldo_bcb),
         var_saldo_bru = saldo_bru - lag(saldo_bru),
         inadimplencia = coalesce(inadimplencia, 0),
         bp = ifelse(
           date <= as.Date("2024-12-01"),
           (0.1*inadimplencia/100) * saldo,
           (0.07* inadimplencia/100) * saldo
         ),
         # bp = 0,
         pgto_bcb = -(concessoes_bcb - var_saldo_bcb + JUROS - bp) ,
         pgto_bru = -(AMORTIZACAO + JUROS),
         ff_bcb = concessoes_bcb + pgto_bcb,
         ff_bru = concessoes_bru + pgto_bru,
  ) |> 
  ungroup() |> 
  rename(concessoes_orig = concessoes) |> 
  select(date, modalidade, saldo_bcb, JUROS, AMORTIZACAO, concessoes_bcb, concessoes_bru, concessoes_orig, pgto_bcb, pgto_bru, ff_bcb, ff_bru)

precos_de <- max(df_fluxo_financeiro$date)


total <- df_fluxo_financeiro |>
  mutate(modalidade_agregada = coalesce(modalidades_agregadas[modalidade], "Outros")) |> 
  group_by(date, modalidade_agregada) |> 
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )|> 
  filter(saldo_bcb > 0)|>
  left_join(dados_macro, by = join_by(date))

# colnames(total)
# modalidade_selecionada <-   "Aquisição de bens"

vars_mm3m <- c("AMORTIZACAO", "JUROS",
               "concessoes_bcb", "concessoes_bru",
               "pgto_bcb", "pgto_bru",
               "ff_bcb", "ff_bru",
               "PIB", "RNDBF")

vars_acum12m <- c("PIB", "RNDBF")

vars_impulso <- c("mm3m_pgto_bcb", "mm3m_pgto_bru",
                  "mm3m_concessoes_bcb", "mm3m_concessoes_bru",
                  "mm3m_ff_bcb", "mm3m_ff_bru")

for (modalidade_selecionada in modalidade_modelo) {
  
  print(modalidade_selecionada)
  
  df_data_def <- total |> 
    filter(modalidade_agregada ==
             modalidade_selecionada
    ) |>
    as_tsibble(index = date) |>  # <- Define 'date' como índice corretamente
    dplyr::select(-modalidade_agregada) %>%
    ungroup() |>
    arrange(date) %>%
    # -------- DEFLACIONA --------
  mutate(
    across(
      where(is.numeric),
      ~ .x * IPCA[date == precos_de][1] / IPCA
    )
  ) |> 
    # -------- MÉDIA MÓVEL 3 M --------
  mutate(
    across( all_of(vars_mm3m), 
            ~ rollmean(.x, k = 3, fill = NA, align = "right"),
            .names = "mm3m_{.col}")
  ) |>
    # -------- ACUMULADO 12 M --------
  
  mutate(
    across( all_of(vars_acum12m), 
            ~ rollsum(.x, k = 12, fill = NA, align = "right"),
            .names = "acum_12m_{.col}")
  ) |>
    # -------- IMPULSO 3M --------
  
  mutate(
    across( all_of(vars_impulso), 
            ~ (.x - lag(.x, 12)),
            .names = "impulso_{.col}")
  ) |>
    #cria variáveis para impulso comparável ao do BNDES
    mutate(mm12m_ff_bcb = rollsum(ff_bcb, k = 12, fill = NA, align = "right"),
           impulso_mm12m_ff_bcb = mm12m_ff_bcb - lag(mm12m_ff_bcb, 12),
           impulso_PIB_12m_bcb = impulso_mm12m_ff_bcb/acum_12m_PIB,
           impulso_PIB_3m_bcb = impulso_mm3m_ff_bcb/mm3m_PIB,
           
           mm12m_ff_bru = rollsum(ff_bru, k = 12, fill = NA, align = "right"),
           impulso_mm12m_ff_bru = mm12m_ff_bru - lag(mm12m_ff_bru, 12),
           impulso_PIB_12m_bru = impulso_mm12m_ff_bru/acum_12m_PIB,
           impulso_PIB_3m_bru = impulso_mm3m_ff_bru/mm3m_PIB)
           

           
  
  
  resultados <- df_data_def 
  
  comp_modalidade_def[[modalidade_selecionada]] <- resultados
  
}

comp_modalidade_total_def <- map2_df(comp_modalidade_def, names(comp_modalidade_def), ~ mutate(as_tibble(.x), origem = .y))
table(comp_modalidade_total_def$origem)


#####save_the_data####


readr::write_excel_csv2(comp_modalidade_total_def,
                        file = paste0("./Output/comp_modalidade_total_def_",  date_file, ".csv"))


readr::write_rds(comp_modalidade_total_def,
                 file = paste0("./Output/comp_modalidade_total_def_",  date_file, ".rds"))


colnames(comp_modalidade_total_def)

comp_modalidade_total_soma_def <- comp_modalidade_total_def %>%
  filter(date >= "2014-03-01") %>%
  group_by(date) |> 
  summarise(total_impulso_mm3m_pgto_bcb = sum(impulso_mm3m_pgto_bcb, na.rm = TRUE),
            total_impulso_mm3m_pgto_bru = sum(impulso_mm3m_pgto_bru, na.rm = TRUE),
            total_impulso_mm3m_concessoes_bcb = sum(impulso_mm3m_concessoes_bcb, na.rm = TRUE),
            total_impulso_mm3m_concessoes_bru = sum(impulso_mm3m_concessoes_bru, na.rm = TRUE),
            total_impulso_mm3m_ff_bcb = sum(impulso_mm3m_ff_bcb, na.rm = TRUE),
            total_impulso_mm3m_ff_bru = sum(impulso_mm3m_ff_bru, na.rm = TRUE),
            total_ff_bcb = sum(ff_bcb, na.rm = TRUE),
            total_mm3m_ff_bcb = sum(mm3m_ff_bcb, na.rm = TRUE),
            total_pgto_bcb = sum(pgto_bcb, na.rm = TRUE),
            total_concessoes_bcb = sum(concessoes_bcb, na.rm = TRUE),
            total_ff_bru = sum(ff_bru, na.rm = TRUE),
            total_mm3m_ff_bru = sum(mm3m_ff_bru, na.rm = TRUE),
            total_pgto_bru = sum(pgto_bru, na.rm = TRUE),
            total_concessoes_bru = sum(concessoes_bru, na.rm = TRUE),
            total_impulso_PIB_12m_bcb = sum(impulso_PIB_12m_bcb, na.rm = TRUE),
            total_impulso_PIB_3m_bcb = sum(impulso_PIB_3m_bcb, na.rm = TRUE),
            total_impulso_PIB_12m_bru = sum(impulso_PIB_12m_bru, na.rm = TRUE),
            total_impulso_PIB_3m_bru = sum(impulso_PIB_3m_bru, na.rm = TRUE)
  )


####PLOTS####
fonte_dados = c("bru", "bcb")

#####TOTAL####
{
fonte_dados <- "bru"
plot_fluxo_financeiro_total_estoque <- ggplot(comp_modalidade_total_soma_def |> 
                                        filter(date >= "2014-01-01"), aes(x = date)) +
  geom_bar(aes(y = .data[[paste0("total_ff_", fonte_dados)]], fill = "Fluxo Financeiro"), stat = "identity",alpha = 0.5) +
  geom_line(aes(y = -(.data[[paste0("total_pgto_", fonte_dados)]]), color = "Pagamentos"), linewidth = 1) +
  geom_line(aes(y = .data[[paste0("total_concessoes_", fonte_dados)]], color = "Concessões"), linewidth = 1) +
  scale_x_date(date_labels = "%Y",breaks = "2 years") +  # Formato do eixo X
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Concessões" = "darkgreen",
               "Pagamentos" = "darkorange", 
               "MM3m | Fluxo Financeiro" = "darkgrey"),  # Ordem desejada
    breaks = c("Concessões","Pagamentos",  "MM3m | Fluxo Financeiro")  # Define ordem na legenda
  ) +  
  scale_fill_manual(
    values = c("Fluxo Financeiro" = "darkgrey"),  # Adicionando a legenda para geom_bar()
    guide = guide_legend(override.aes = list(alpha = 0.5))
  ) +
  
  labs(
    title = "Abordagem de Estoque",
    x = "Data",
    y = "Pagamentos, Concessões \n e Fluxo Financeiro (R$ milhões)",
    color = "Legenda") +  # Nome da legenda
  theme_minimal() + 
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_blank() ,
    panel.grid.minor = element_blank(),                    # remove grades menores
    panel.grid.major.x = element_line(color = "grey80"),   # mantém apenas a grade principal em x
    panel.grid.major.y = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )
}
plot_fluxo_financeiro_total_estoque

{
fonte_dados <- "bcb"
plot_fluxo_financeiro_total_contabil <- ggplot(comp_modalidade_total_soma_def |> 
                                                filter(date >= "2014-01-01"), aes(x = date)) +
  geom_bar(aes(y = .data[[paste0("total_ff_", fonte_dados)]], fill = "Fluxo Financeiro"), stat = "identity",alpha = 0.5) +
  geom_line(aes(y = -(.data[[paste0("total_pgto_", fonte_dados)]]), color = "Pagamentos"), linewidth = 1) +
  geom_line(aes(y = .data[[paste0("total_concessoes_", fonte_dados)]], color = "Concessões"), linewidth = 1) +
  scale_x_date(date_labels = "%Y",breaks = "2 years") +  # Formato do eixo X
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Concessões" = "darkgreen",
               "Pagamentos" = "darkorange", 
               "MM3m | Fluxo Financeiro" = "darkgrey"),  # Ordem desejada
    breaks = c("Concessões","Pagamentos",  "MM3m | Fluxo Financeiro")  # Define ordem na legenda
  ) +  
  scale_fill_manual(
    values = c("Fluxo Financeiro" = "darkgrey"),  # Adicionando a legenda para geom_bar()
    guide = guide_legend(override.aes = list(alpha = 0.5))
  ) +
  
  labs(
    title = "Abordagem Contábil",
    x = "Data",
    y = "Pagamentos, Concessões \n e Fluxo Financeiro (R$)",
    color = "Legenda") +  # Nome da legenda
  theme_minimal() + 
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_blank() ,
    panel.grid.minor = element_blank(),                    # remove grades menores
    panel.grid.major.x = element_line(color = "grey80"),   # mantém apenas a grade principal em x
    panel.grid.major.y = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5)
  )
}

plot_fluxo_financeiro_total_contabil



# Supondo que você tenha dois objetos de gráfico: plot1 e plot2
# Lado a lado:
p1 <- plot_fluxo_financeiro_total_contabil + 
  theme(legend.position = "none")
p2 <- plot_fluxo_financeiro_total_estoque + 
  labs(y = NULL) +                # Remove o título do eixo Y do segundo gráfico
  theme(legend.position = "none")

plot_final_fluxo_financeiro <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

plot_final_fluxo_financeiro

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_final_fluxo_financeiro.svg"),
  plot = plot_final_fluxo_financeiro, width = 16, height = 6, dpi = 1000)


####IMPULSO####

#TOTAL
# comp_modalidade_total_soma_long_def <- comp_modalidade_total_soma_def |> 
#   filter(date >= "2014-03-01") |> 
#   pivot_longer(cols = c(total_impulso_con, total_impulso_pmt, total_impulso_amort, total_impulso_juros, total_impulso, total_impulso_PIB, total_impulso_PIB_3m ), 
#                names_to = "categoria", 
#                values_to = "valor")

comp_modalidade_total_soma_long_def <- comp_modalidade_total_soma_def |> 
  filter(date >= "2015-01-01") |> 
  pivot_longer(!date,
               names_to = "categoria", 
               values_to = "valor")

colnames(comp_modalidade_total_def)

comp_modalidade_total_long_def <- comp_modalidade_total_def |> 
  select(date, origem, impulso_mm3m_ff_bcb , impulso_mm3m_concessoes_bcb, impulso_mm3m_pgto_bcb,
                       impulso_mm3m_ff_bru , impulso_mm3m_concessoes_bru, impulso_mm3m_pgto_bru,) |> 
  filter(date >= "2015-01-01") |> 
  pivot_longer(!c(date,origem),
               names_to = "categoria", 
               values_to = "valor")


#####CONCESSÕES E PMT#####
######TOTAL####


{
fonte_dados <- "bcb"
cat_conc <- paste0("total_impulso_mm3m_concessoes_", fonte_dados)
cat_pgto <- paste0("total_impulso_mm3m_pgto_", fonte_dados)
cat_total <- paste0("total_impulso_mm3m_ff_", fonte_dados)
# Criar gráfico empilhado com linha separada
plot_impulso_credito_total_contabil <- ggplot() +
  # Barras empilhadas
  geom_bar(data = comp_modalidade_total_soma_long_def |> 
             filter(date >= "2015-01-01") |> 
             filter(categoria %in% c(cat_conc, cat_pgto)), aes(x = date, y = valor, fill = categoria), 
           stat = "identity", position = "stack", alpha = 0.7) +
  # Linha separada
  geom_line(data = comp_modalidade_total_soma_def |> 
              filter(date >= "2015-01-01") , aes(x = date,
                                                 y = .data[[cat_total]],
                                                 color = "Total"), 
            size = 1) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0)) + # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_fill_manual(
    values = setNames(
      c("darkgreen", "darkorange"),
      c(cat_conc, cat_pgto)
    ),
    labels = setNames(
      c("Impulso Concessões", "Impulso Serviço da Dívida"),
      c(cat_conc, cat_pgto)
    )
  )  +  
  scale_color_manual(values = c("Total" = "black"),
                     labels = c("Total" = "Impulso Total de Crédito")) +  # Define cor da linha total
  labs(    title = "Abordagem Contábil",
           x = "Data",
       y = "Impulso de Crédito (R$ milhões)",
       fill = "",
       color = "") +
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
    plot.title = element_text(hjust = 0.5)
  )
}

plot_impulso_credito_total_contabil

{
fonte_dados <- "bru"
cat_conc <- paste0("total_impulso_mm3m_concessoes_", fonte_dados)
cat_pgto <- paste0("total_impulso_mm3m_pgto_", fonte_dados)
cat_total <- paste0("total_impulso_mm3m_ff_", fonte_dados)
# Criar gráfico empilhado com linha separada
plot_impulso_credito_total_estoque <- ggplot() +
  # Barras empilhadas
  geom_bar(data = comp_modalidade_total_soma_long_def |> 
             filter(date >= "2015-01-01") |> 
             filter(categoria %in% c(cat_conc, cat_pgto)), aes(x = date, y = valor, fill = categoria), 
           stat = "identity", position = "stack", alpha = 0.7) +
  # Linha separada
  geom_line(data = comp_modalidade_total_soma_def |> 
              filter(date >= "2015-01-01") , aes(x = date,
                                                 y = .data[[cat_total]],
                                                 color = "Total"), 
            size = 1) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0)) + # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_fill_manual(
    values = setNames(
      c("darkgreen", "darkorange"),
      c(cat_conc, cat_pgto)
    ),
    labels = setNames(
      c("Impulso Concessões", "Impulso Serviço da Dívida"),
      c(cat_conc, cat_pgto)
    )
  )  +  
  scale_color_manual(values = c("Total" = "black"),
                     labels = c("Total" = "Impulso Total de Crédito")) +  # Define cor da linha total
  labs(    title = "Abordagem de Estoque",
           x = "Data",
           y = "Impulso de Crédito (R$ milhões)",
           fill = "",
           color = "") +
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
    plot.title = element_text(hjust = 0.5)
  )
}

plot_impulso_credito_total_estoque

# Supondo que você tenha dois objetos de gráfico: plot1 e plot2
# Lado a lado:
p1 <- plot_impulso_credito_total_contabil + 
  theme(legend.position = "none")

p2 <- plot_impulso_credito_total_estoque + 
  labs(y = NULL) +                # Remove o título do eixo Y do segundo gráfico
  theme(legend.position = "none")

plot_final_impulso_credito <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

plot_final_impulso_credito

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_final_impulso_credito.svg"),
  plot = plot_final_impulso_credito, width = 16, height = 6, dpi = 1000)


######MODALIDADES####
{fonte_dados <- "bcb"

cat_conc <- paste0("impulso_mm3m_concessoes_", fonte_dados)
cat_pgto <- paste0("impulso_mm3m_pgto_", fonte_dados)
cat_total <- paste0("impulso_mm3m_ff_", fonte_dados)

# Criar gráfico empilhado com linha separada
plot_impulso_modalidades_contabil <- ggplot() +
  # Barras empilhadas
  geom_bar(data = comp_modalidade_total_long_def |> 
             filter(categoria %in%  c(cat_conc, cat_pgto)), 
           aes(x = date, y = valor, fill = categoria), 
           stat = "identity", position = "stack", alpha = 0.7) +
  
  # Linha separada
  geom_line(data = comp_modalidade_total_long_def |> filter(categoria == cat_total), 
            aes(x = date, y = valor, color = "Total"), size = 1) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0)) + # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  # Renomeia as categorias na legenda e define cores
  scale_fill_manual(
    values = setNames(
      c("darkgreen", "darkorange"),
      c(cat_conc, cat_pgto)
    ),
    labels = setNames(
      c("Impulso Concessões", "Impulso Serviço da Dívida"),
      c(cat_conc, cat_pgto)
    )
  )  +  
  
  # Define cor da linha total e nome correto na legenda
  scale_color_manual(
    values = c("Total" = "black"),
    labels = c("Total" = "Impulso Total de Crédito")
  ) +  
  
  labs(  title = "Abordagem Contábil",
         x = "Data",
       y = "Impulso de Crédito (R$ milhões)",
       fill = "",
       color = "") +
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
    plot.title = element_text(hjust = 0.5)
    
  )+
  facet_wrap(~origem, ncol = 1, scales = "free_y")
}
plot_impulso_modalidades_contabil


######MODALIDADES####
{
  fonte_dados <- "bru"

cat_conc <- paste0("impulso_mm3m_concessoes_", fonte_dados)
cat_pgto <- paste0("impulso_mm3m_pgto_", fonte_dados)
cat_total <- paste0("impulso_mm3m_ff_", fonte_dados)


# Criar gráfico empilhado com linha separada
plot_impulso_modalidades_estoque <- ggplot() +
  # Barras empilhadas
  geom_bar(data = comp_modalidade_total_long_def |> 
             filter(categoria %in%  c(cat_conc, cat_pgto)), 
           aes(x = date, y = valor, fill = categoria), 
           stat = "identity", position = "stack", alpha = 0.7) +
  
  # Linha separada
  geom_line(data = comp_modalidade_total_long_def |> filter(categoria == cat_total), 
            aes(x = date, y = valor, color = "Total"), size = 1) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year", expand = c(0,0)) + # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  # Renomeia as categorias na legenda e define cores
  scale_fill_manual(
    values = setNames(
      c("darkgreen", "darkorange"),
      c(cat_conc, cat_pgto)
    ),
    labels = setNames(
      c("Impulso Concessões", "Impulso Serviço da Dívida"),
      c(cat_conc, cat_pgto)
    )
  )  +  
  
  # Define cor da linha total e nome correto na legenda
  scale_color_manual(
    values = c("Total" = "black"),
    labels = c("Total" = "Impulso Total de Crédito")
  ) +  
  
  labs(  title = "Abordagem de Estoque",
         x = "Data",
         y = "Impulso de Crédito (R$ milhões)",
         fill = "",
       color = "") +
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
    plot.title = element_text(hjust = 0.5)
    
  )+
  facet_wrap(~origem, ncol = 1, scales = "free_y")
}

plot_impulso_modalidades_estoque

# Lado a lado:
p1 <- plot_impulso_modalidades_contabil + 
  theme(legend.position = "none")

p2 <- plot_impulso_modalidades_estoque + 
  labs(y = NULL) +                # Remove o título do eixo Y do segundo gráfico
  theme(legend.position = "none")

plot_final_impulso_modalidades <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

plot_final_impulso_modalidades

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_final_impulso_modalidades.svg"),
  plot = plot_final_impulso_modalidades, width = 16, height = 16, dpi = 1000)

#####IMPULSO 3 MESES %PIB####

{
fonte_dados <- "bcb" 
# Criando o gráfico base
plot_impulso_pib3m_contabil <- ggplot() +
  # 1. Barras Empilhadas (equivalente ao add_bars com barmode = "relative")
  geom_col(
    data = comp_modalidade_total_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("impulso_PIB_3m_", fonte_dados)]] * 100, 
      fill = origem
    ),
    position = "stack"
  ) +
  # 2. Linha da Soma Total
  geom_line(
    data = comp_modalidade_total_soma_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("total_impulso_PIB_3m_", fonte_dados)]] * 100,
      color = "Total" # Criamos uma legenda para a linha aqui
    ),
    linewidth = 1
  ) +
  # Configuração de Cores e Escalas
  scale_fill_manual(values = cores_modalidades) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year") +
  # Estética e Títulos
  labs(
    title = "Abordagem Contábil",
    x = "Data",
    y = "% PIB",
    fill = "Modalidade",
    color = ""
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 10, 10) # Margem à direita para não cortar
  )
}
# Visualizar
plot_impulso_pib3m_contabil

{
fonte_dados <- "bru" 
# Criando o gráfico base
plot_impulso_pib3m_estoque <- ggplot() +
  # 1. Barras Empilhadas (equivalente ao add_bars com barmode = "relative")
  geom_col(
    data = comp_modalidade_total_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("impulso_PIB_3m_", fonte_dados)]] * 100, 
      fill = origem
    ),
    position = "stack"
  ) +
  # 2. Linha da Soma Total
  geom_line(
    data = comp_modalidade_total_soma_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("total_impulso_PIB_3m_", fonte_dados)]] * 100,
      color = "Total" # Criamos uma legenda para a linha aqui
    ),
    linewidth = 1
  ) +
  # Configuração de Cores e Escalas
  scale_fill_manual(values = cores_modalidades) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year") +
  # Estética e Títulos
  labs(
    title = "Abordagem de Estoque",
    x = "Data",
    y = "% PIB",
    fill = "Modalidade",
    color = ""
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 10, 10) # Margem à direita para não cortar
  )
}
# Visualizar
plot_impulso_pib3m_estoque

limites_y <- c(-6, + 6)

# Lado a lado:
p1 <- plot_impulso_pib3m_contabil + 
  scale_y_continuous(limits = limites_y)+ 
  theme(legend.position = "none")

p2 <- plot_impulso_pib3m_estoque + 
  scale_y_continuous(limits = limites_y) + 
  labs(y = NULL) +                # Remove o título do eixo Y do segundo gráfico
  theme(legend.position = "none")

plot_impulso_pib3m <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

plot_impulso_pib3m

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib3m.svg"),
  plot = plot_impulso_pib3m, width = 16, height = 8, dpi = 1000)



#####IMPULSO 12 MESES %PIB####
{
fonte_dados <- "bcb" 
# Criando o gráfico base
plot_impulso_pib12m_contabil <- ggplot() +
  # 1. Barras Empilhadas (equivalente ao add_bars com barmode = "relative")
  geom_col(
    data = comp_modalidade_total_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("impulso_PIB_12m_", fonte_dados)]] * 100, 
      fill = origem
    ),
    position = "stack"
  ) +
  # 2. Linha da Soma Total
  geom_line(
    data = comp_modalidade_total_soma_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("total_impulso_PIB_12m_", fonte_dados)]] * 100,
      color = "Total" # Criamos uma legenda para a linha aqui
    ),
    linewidth = 1
  ) +
  # Configuração de Cores e Escalas
  scale_fill_manual(values = cores_modalidades) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year") +
  # Estética e Títulos
  labs(
    title = "Abordagem Contábil",
    x = "Data",
    y = "% PIB",
    fill = "Modalidade",
    color = ""
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 10, 10) # Margem à direita para não cortar
  )
}
# Visualizar
plot_impulso_pib12m_contabil
{
fonte_dados <- "bru" 
# Criando o gráfico base
plot_impulso_pib12m_estoque <- ggplot() +
  # 1. Barras Empilhadas (equivalente ao add_bars com barmode = "relative")
  geom_col(
    data = comp_modalidade_total_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("impulso_PIB_12m_", fonte_dados)]] * 100, 
      fill = origem
    ),
    position = "stack"
  ) +
  # 2. Linha da Soma Total
  geom_line(
    data = comp_modalidade_total_soma_def |> filter(date >= as.Date("2015-01-01")),
    aes(
      x = date, 
      y = .data[[paste0("total_impulso_PIB_12m_", fonte_dados)]] * 100,
      color = "Total" # Criamos uma legenda para a linha aqui
    ),
    linewidth = 1
  ) +
  # Configuração de Cores e Escalas
  scale_fill_manual(values = cores_modalidades) +
  scale_color_manual(values = c("Total" = "black")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 year") +
  # Estética e Títulos
  labs(
    title = "Abordagem de Estoque",
    x = "Data",
    y = "% PIB",
    fill = "Modalidade",
    color = ""
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 10, 10) # Margem à direita para não cortar
  )
}
# Visualizar
plot_impulso_pib12m_estoque

limites_y <- c(-4, + 3)

# Lado a lado:
p1 <- plot_impulso_pib12m_contabil + 
  scale_y_continuous(limits = limites_y)+ 
  theme(legend.position = "none")

p2 <- plot_impulso_pib12m_estoque + 
  scale_y_continuous(limits = limites_y) + 
  labs(y = NULL) +                # Remove o título do eixo Y do segundo gráfico
  theme(legend.position = "none")

plot_impulso_pib12m <- (p1 | p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

plot_impulso_pib12m

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib12m.svg"),
  plot = plot_impulso_pib12m, width = 16, height = 8, dpi = 1000)


####IMPULSO PLOTLY####
######IMPULSO EM %PIB - 3m####
{
fonte_dados <- "bcb" 

plot_impulso_pib3m_contabil <- plot_ly() %>%
  # Adiciona as barras empilhadas por origem
  add_bars(
    data = comp_modalidade_total_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("impulso_PIB_3m_", fonte_dados)]])*100,
    color = ~origem,
    colors = cores_modalidades,
    text = ~paste0(round((.data[[paste0("impulso_PIB_3m_", fonte_dados)]]), 1)), # Adiciona rótulos de porcentagem
    textposition = "inside"
  ) %>%
  # Adiciona a linha da soma total
  add_lines(
    data = comp_modalidade_total_soma_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("total_impulso_PIB_3m_", fonte_dados)]])*100,
    name = "Total", # Nome da linha na legenda
    line = list(color = "black", width = 2, dash = "solid") # Estiliza a linha
  ) %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "% PIB", font = list(size = 20))), # Ajusta para começar do zero
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    barmode = "relative" # Barras empilhadas
  )
}
plot_impulso_pib3m_contabil

exportar_plotly_png(plot_impulso_pib3m_contabil, 
                    file = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib3m_contabil.png"), 
                    width = 1600, 
                    height = 800)


{
fonte_dados <- "bru" 
plot_impulso_pib3m_estoque <- plot_ly() %>%
  # Adiciona as barras empilhadas por origem
  add_bars(
    data = comp_modalidade_total_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("impulso_PIB_3m_", fonte_dados)]])*100,
    color = ~origem,
    colors = cores_modalidades,
    text = ~paste0(round((.data[[paste0("impulso_PIB_3m_", fonte_dados)]]), 1)), # Adiciona rótulos de porcentagem
    textposition = "inside"
  ) %>%
  # Adiciona a linha da soma total
  add_lines(
    data = comp_modalidade_total_soma_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("total_impulso_PIB_3m_", fonte_dados)]])*100,
    name = "Total", # Nome da linha na legenda
    line = list(color = "black", width = 2, dash = "solid") # Estiliza a linha
  ) %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "% PIB", font = list(size = 20))), # Ajusta para começar do zero
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    barmode = "relative" # Barras empilhadas
  )

}
plot_impulso_pib3m_estoque

exportar_plotly_png(plot_impulso_pib3m_estoque, 
                    file = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib3m_estoque.png"), 
                    width = 1600, 
                    height = 800)


######IMPULSO EM %PIB - 12m####
{fonte_dados <- "bru" 
plot_impulso_pib12m_estoque <- plot_ly() %>%
  # Adiciona as barras empilhadas por origem
  add_bars(
    data = comp_modalidade_total_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("impulso_PIB_12m_", fonte_dados)]])*100,
    color = ~origem,
    colors = cores_modalidades,
    text = ~paste0(round((.data[[paste0("impulso_PIB_12m_", fonte_dados)]]), 1)), # Adiciona rótulos de porcentagem
    textposition = "inside"
  ) %>%
  # Adiciona a linha da soma total
  add_lines(
    data = comp_modalidade_total_soma_def |> filter(date >= "2015-01-01"),
    x = ~date,
    y = ~(.data[[paste0("total_impulso_PIB_12m_", fonte_dados)]])*100,
    name = "Total", # Nome da linha na legenda
    line = list(color = "black", width = 2, dash = "solid") # Estiliza a linha
  ) %>%
  layout(
    title = list(text = ""), 
    xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
    yaxis = list(title = list(text = "% PIB", font = list(size = 20))), # Ajusta para começar do zero
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    barmode = "relative" # Barras empilhadas
  )
}

  plot_impulso_pib12m_estoque

  exportar_plotly_png(plot_impulso_pib12m_estoque, 
                      file = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib12m_estoque.png"), 
                      width = 1600, 
                      height = 800)

{fonte_dados <- "bcb" 
  plot_impulso_pib12m_contabil <- plot_ly() %>%
    # Adiciona as barras empilhadas por origem
    add_bars(
      data = comp_modalidade_total_def |> filter(date >= "2015-01-01"),
      x = ~date,
      y = ~(.data[[paste0("impulso_PIB_12m_", fonte_dados)]])*100,
      color = ~origem,
      colors = cores_modalidades,
      text = ~paste0(round((.data[[paste0("impulso_PIB_12m_", fonte_dados)]]), 1)), # Adiciona rótulos de porcentagem
      textposition = "inside"
    ) %>%
    # Adiciona a linha da soma total
    add_lines(
      data = comp_modalidade_total_soma_def |> filter(date >= "2015-01-01"),
      x = ~date,
      y = ~(.data[[paste0("total_impulso_PIB_12m_", fonte_dados)]])*100,
      name = "Total", # Nome da linha na legenda
      line = list(color = "black", width = 2, dash = "solid") # Estiliza a linha
    ) %>%
    layout(
      title = list(text = ""), 
      xaxis = list(title = list(text = "Data",  font = list(size = 20)), tickformat = "%b\n%Y", showgrid = TRUE), 
      yaxis = list(title = list(text = "% PIB", font = list(size = 20))), # Ajusta para começar do zero
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
      barmode = "relative" # Barras empilhadas
    )
}

plot_impulso_pib12m_contabil

exportar_plotly_png(plot_impulso_pib12m_contabil, 
                    file = paste0(".\\Output\\Plots\\", chapter_section, "plot_impulso_pib12m_contabil.png"), 
                    width = 1600, 
                    height = 800)




dados_macro <-  readr::read_rds(file = paste0("./Output/dados_macro_",  date_file, ".rds")) |> 
  select(-IBCBR, -RNDBF_ajustado, -SELIC)


ff_bcb_rpm <- readxl::read_xlsx(".\\Input\\rpm202606anp.xlsx", sheet = "Graf 1.2.30", skip = 8) |>
  select(`Mês`, `Pessoas físicas`) |> 
  rename(date = `Mês`,
         ff_bcb_rpm = `Pessoas físicas`) |> 
  mutate(date = as.Date(date))

ff_pib <- df_fluxo_financeiro |> 
  select(date, ff_bcb, ff_bru) |> 
  group_by(date) |> 
  summarise(ff_bcb = sum(ff_bcb, na.rm = TRUE),
            ff_bru = sum(ff_bru, na.rm = TRUE)) |> 
  mutate(ff_bcb_acum_12 = rollsum(ff_bcb, k = 12, fill = NA, align = "right"),
         ff_bru_acum_12 = rollsum(ff_bru, k = 12, fill = NA, align = "right")) |> 
  drop_na() |> 
  left_join(dados_macro) |> 
  mutate(ff_bcb_pib = (ff_bcb_acum_12/PIB_acum12m)*100,
         ff_bru_pib = (ff_bru_acum_12/PIB_acum12m)*100) |> 
  select(date, ff_bcb_pib, ff_bru_pib) |> 
  filter(date >= "2014-01-01") |>
  left_join(ff_bcb_rpm) |> 
  pivot_longer(cols = c(ff_bcb_pib, ff_bru_pib, ff_bcb_rpm))


plot_ff_pib12m <- ggplot() +
  # 🔵 Barras
  geom_line(
    data = ff_pib |> 
      filter(name == "ff_bcb_pib"),
    aes(x = date, y = value, color = "Abordagem Contábil"), linewidth = 0.8
  ) +
  # 🔵 Barras
  geom_line(
    data = ff_pib |> 
      filter(name == "ff_bru_pib"),
    aes(x = date, y = value, color = "Abordagem Estoque"), linewidth = 0.8
  )   +
  # 🔵 Barras
  geom_line(
    data = ff_pib |> 
      filter(name == "ff_bcb_rpm"),
    aes(x = date, y = value, color = "BCB (Relatório de Política Monetária)"), linewidth = 0.8
  )   +
  
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(
    breaks = function(limits) seq(floor(limits[1]), ceiling(limits[2]), by = 1)
  )+
  scale_color_manual(
    values = c("Abordagem Contábil" = "blue3",
               "Abordagem Estoque" = "red4",
               "BCB (Relatório de Política Monetária)" = "red"),  # Ordem desejada
    breaks = c("Abordagem Contábil","Abordagem Estoque","BCB (Relatório de Política Monetária)")  # Define ordem na legenda
  ) +
  scale_fill_manual(values = "darkgray") +
  labs(x = "Data",
       y = "% PIB",
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
  )

plot_ff_pib12m

ggsave(
  filename = paste0(".\\Output\\Plots\\", chapter_section, "plot_ff_pib12m.svg"),
  plot = plot_ff_pib12m, width = 16, height = 6, dpi = 1000)


readr::write_excel_csv2(ff_pib,
                        file = paste0("./Output/ff_pib.csv"))



