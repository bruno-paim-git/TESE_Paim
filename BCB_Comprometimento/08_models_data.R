# OBJETIVO:
# Consolida-se as bases que serão utilizadas para rodar os modelos econométricos do capítulo posterior

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

dados_macro <-  readr::read_rds(file = paste0("./Output/dados_macro_",  date_file, ".rds")) |> 
  select(date, PIB, SELIC,  IPCA)

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




df_fluxo_financeiro <- readr::read_rds(file = paste0("./Output/df_fluxo_financeiro_",  date_file, ".rds")) |> 
  select(date, modalidade, saldo_bcb, saldo_bru, tx_juros, concessoes_orig, concessoes_bcb, concessoes_bru, icc_am, pgto_bcb, pgto_bru, ff_bcb, ff_bru)

precos_de <- max(df_fluxo_financeiro$date)

####PRINCIPAL ARQUIVO####

total <- df_fluxo_financeiro |>
  mutate(
    modalidade_agregada = coalesce(modalidades_agregadas[modalidade], "Outros"),
  ) |>
  group_by(date, modalidade_agregada) |>
  summarise(
    # -------- SOMAS --------
    across(
      c(
        saldo_bcb, saldo_bru,
        pgto_bcb, pgto_bru,
        ff_bcb, ff_bru,
        concessoes_bcb, concessoes_bru, concessoes_orig
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # -------- MÉDIAS PONDERADAS CONDICIONAIS --------
    TX_JUROS = sum(tx_juros * concessoes_bru, na.rm = TRUE) / sum(concessoes_bru, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  filter(saldo_bru > 0)

# esquisse::esquisser(total)

# Cria funções para dessazonalizar, logaritmizar e diferenciar

dessaz_safe <- function(x, start = c(2015, 1), freq = 12) {
  
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


log_safe <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & x > 0
  out[ok] <- log(x[ok])
  out
}

diff_safe <- function(x, lag = 1) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  
  if (sum(ok) <= lag) return(out)
  
  out[ok] <- c(rep(NA_real_, lag), diff(x[ok], lag = lag))
  out
}

# seleciona as variáveis que serão deflacionadas, logaritmizadas, dessazonalizadas e diferenciadas

vars_deflacionar <- c(
  "pgto_bcb", "pgto_bru",
  "ff_bcb", "ff_bru",
  "concessoes_bcb", "concessoes_bru", "concessoes_orig",
  "PIB"
)

vars_log <- c(
  "pgto_bcb", "pgto_bru",
  "concessoes_bcb", "concessoes_bru", "concessoes_orig",
  "PIB"
  
)

vars_dessaz <- c(
  "log_pgto_bcb", "log_pgto_bru",
  "log_concessoes_bcb", "log_concessoes_bru", "log_concessoes_orig",
  "log_PIB"
)

vars_diff <- c(
  "sa_log_pgto_bcb",
  "sa_log_pgto_bru",
  "sa_log_concessoes_bcb",
  "sa_log_concessoes_bru",
  "sa_log_concessoes_orig",
  "sa_log_PIB",
  "TX_JUROS", "SELIC"
)


# Aplica funções à base completa, criando novas colunas
df_data_model <- total |>
  filter(date >= "2015-01-01") |> 
  left_join(dados_macro, by = join_by(date)) |> 
  group_by(modalidade_agregada) |>  
  as_tsibble(index = date, key = modalidade_agregada) |>  
  
  dplyr::select(
    date, modalidade_agregada,
    SELIC, TX_JUROS, IPCA, PIB,
    all_of(vars_deflacionar)
  ) |>
  
  drop_na(-TX_JUROS) |> 
  
  # -------- DEFLACIONA --------
mutate(
  across(
    all_of(vars_deflacionar),
    ~ .x * IPCA[date == precos_de][1] / IPCA
  )
) |> 
  
  # -------- LOGARITMO --------
mutate(
  across(all_of(vars_log), 
         log_safe,
         .names = "log_{.col}")
) |>
  # -------- DESSAZONALIZA --------
mutate(
  across(
    all_of(vars_dessaz),
    dessaz_safe,
    .names = "sa_{.col}"
  )) |> 
  # -------- IDENTIDADES --------
mutate(
  ff_bcb = concessoes_bcb - pgto_bcb,
  ff_bru = concessoes_bru - pgto_bru
) |> 
  # -------- DIFERENCIA --------
mutate(
  across(
    all_of(vars_diff),
    diff_safe,
    .names = "d_{.col}"
  )
)

df_data_model

#####save_the_data####


readr::write_excel_csv2(df_data_model,
                        file = paste0("./Output/df_data_model_",  date_file, ".csv"))


readr::write_rds(df_data_model,
                 file = paste0("./Output/df_data_model_",  date_file, ".rds"))

####MODELO VEC####


#####GRÁFICO CONCESSOES####
f_chapter <- ".\\Output\\Plots\\chapter_4_1"

plot_concessoes_sa_log <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = sa_log_concessoes_bcb, color = "Abordagem Contábil"), linewidth = 0.8) +
  geom_line(aes(y = sa_log_concessoes_bru, color = "Abordagem Estoque"), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Abordagem Contábil" = "blue3",
               "Abordagem Estoque" = "red4"),  # Ordem desejada
    breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
  ) +
  labs(x = "Data",
       y = "Concessões",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_concessoes_sa_log

ggsave(filename = paste0(f_chapter, "\\VEC_plot_concessoes_sa_log.svg"), 
       plot = plot_concessoes_sa_log, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO PAGAMENTOS####

plot_pagamentos_sa_log <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = sa_log_pgto_bcb, color = "Abordagem Contábil"), linewidth = 0.8) +
  geom_line(aes(y = sa_log_pgto_bru, color = "Abordagem Estoque"), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Abordagem Contábil" = "blue3",
               "Abordagem Estoque" = "red4"),  # Ordem desejada
    breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
  ) +
  labs(x = "Data",
       y = "Pagamentos",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_pagamentos_sa_log

ggsave(filename = paste0(f_chapter, "\\VEC_plot_pagamentos_sa_log.svg"), 
       plot = plot_pagamentos_sa_log, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO TX_JUROS####

plot_tx_juros <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = TX_JUROS), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "Taxa de Juros (%a.a.)",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_tx_juros

ggsave(filename = paste0(f_chapter, "\\VEC_plot_tx_juros.png"), 
       plot = plot_tx_juros, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO SELIC####

plot_selic <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada == "Consignado"), aes(x = date)) +
  geom_line(aes(y = SELIC), linewidth = 1) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "Taxa SELIC (%a.a.)",
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
  )

plot_selic

ggsave(filename = paste0(f_chapter, "\\VEC_plot_selic.png"), 
       plot = plot_selic, 
       width = 6, height = 4, dpi = 500)

#####GRÁFICO PIB####

plot_pib <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada == "Consignado"), aes(x = date)) +
  geom_line(aes(y = sa_log_PIB), linewidth = 1) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "PIB",
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
  )

plot_pib

ggsave(filename = paste0(f_chapter, "\\VEC_plot_pib.png"), 
       plot = plot_pib, 
       width = 6, height = 4, dpi = 500)



####MODELO VAR####


#####GRÁFICO CONCESSOES####
f_chapter <- ".\\Output\\Plots\\chapter_4_1"

plot_concessoes_sa_log <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = d_sa_log_concessoes_bcb, color = "Abordagem Contábil"), linewidth = 0.8) +
  geom_line(aes(y = d_sa_log_concessoes_bru, color = "Abordagem Estoque"), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Abordagem Contábil" = "blue3",
               "Abordagem Estoque" = "red4"),  # Ordem desejada
    breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
  ) +
  labs(x = "Data",
       y = "Concessões",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_concessoes_sa_log

ggsave(filename = paste0(f_chapter, "\\VAR_plot_concessoes_sa_log.svg"), 
       plot = plot_concessoes_sa_log, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO PAGAMENTOS####

plot_pagamentos_sa_log <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = d_sa_log_pgto_bcb, color = "Abordagem Contábil"), linewidth = 0.8) +
  geom_line(aes(y = d_sa_log_pgto_bru, color = "Abordagem Estoque"), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  scale_color_manual(
    values = c("Abordagem Contábil" = "blue3",
               "Abordagem Estoque" = "red4"),  # Ordem desejada
    breaks = c("Abordagem Contábil","Abordagem Estoque")  # Define ordem na legenda
  ) +
  labs(x = "Data",
       y = "Pagamentos",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_pagamentos_sa_log

ggsave(filename = paste0(f_chapter, "\\VAR_plot_pagamentos_sa_log.svg"), 
       plot = plot_pagamentos_sa_log, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO TX_JUROS####

plot_tx_juros <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada != "Outros"), aes(x = date)) +
  geom_line(aes(y = d_TX_JUROS), linewidth = 0.8) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "Taxa de Juros (%a.a.)",
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
  )+
  facet_wrap(~modalidade_agregada, ncol = 2, scales = "free_y")

plot_tx_juros

ggsave(filename = paste0(f_chapter, "\\VAR_plot_tx_juros.png"), 
       plot = plot_tx_juros, 
       width = 12, height = 8, dpi = 1000)

#####GRÁFICO SELIC####

plot_selic <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada == "Consignado"), aes(x = date)) +
  geom_line(aes(y = d_SELIC), linewidth = 1) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "Taxa SELIC (%a.a.)",
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
  )

plot_selic

ggsave(filename = paste0(f_chapter, "\\VAR_plot_selic.png"), 
       plot = plot_selic, 
       width = 6, height = 4, dpi = 500)

#####GRÁFICO PIB####

plot_pib <- ggplot(df_data_model |> filter(date >= "2015-01-01") |> filter(modalidade_agregada == "Consignado"), aes(x = date)) +
  geom_line(aes(y = d_sa_log_PIB), linewidth = 1) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 year", expand = c(0,0)) +  # Remove espaços extras
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ","))+
  labs(x = "Data",
       y = "PIB",
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
  )

plot_pib

ggsave(filename = paste0(f_chapter, "\\VAR_plot_pib.png"), 
       plot = plot_pib, 
       width = 6, height = 4, dpi = 500)
