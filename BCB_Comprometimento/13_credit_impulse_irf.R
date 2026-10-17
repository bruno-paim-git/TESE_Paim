# Rótulos de Linha	Soma de concessoes_bcb	Soma de concessoes_bru	Soma de pgto_bcb	Soma de pgto_bru
# 01/07/2026	            465677	               299246,5785	          481733,4697	    201437,6493
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
  library(ggh4x)
  library(ggpattern)
  library(ggplot2)
  
  options(scipen = 999)
  
  conflict_prefer("layout", "plotly")
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("lag", "dplyr")
  
  source("./function_export_plot.R")
  
}
#abre bases necessárias
df_data_model <-  readr::read_rds(file = paste0("./Output/df_data_model_",  date_file, ".rds")) 

max_date <- max(df_data_model$date)

#resgata valores reais de concessões e pagamentos por modalidade para cada abordagem
df_nivel_base <- df_data_model |> filter(date == max_date) |> 
  select(concessoes_bcb, concessoes_bru, pgto_bcb, pgto_bru)

#utiliza base ajustada para variação de 0,5 p.p. da SELIC
#seleciona o impacto final da IRF, ou seja, no fim do horizonte projetado
#nesse caso, o que se tem é o impacto no fluxo mensal no 12º mês, e não o impacto acumulado 12 meses
#basta retirar o filtro horizon == 12 que o cálculo funcionará para o acumulado


irf_plots_data_VAR <- read_rds(file = ".\\Output\\irf_plots_data_VAR_selecionado.rds") |> 
  mutate(significancia = case_when(Mean > 0 & Low_16 > 0 ~ "sim",
                                   Mean < 0 & Up_84 < 0 ~ "sim", 
                                   .default = "nao")) |> 
  filter(horizon == 12)

irf_plots_data_VEC <- read_rds(file = ".\\Output\\irf_plots_data_VEC_selecionado.rds")|> 
  mutate(significancia = case_when(Mean > 0 & Low_16 > 0 ~ "sim",
                                   Mean < 0 & Up_84 < 0 ~ "sim", 
                                   .default = "nao"))|> 
  filter(horizon == 12)

#filtra variável de impulso
variavel <- "SELIC"

# cria df com indicação da significância da IRF no ponto final considerado
significancia <- rbind(irf_plots_data_VAR, irf_plots_data_VEC) |> 
  filter(impulse == variavel, variable %in% c("CONCESSOES", "PGTO")) |> 
  select(modalidade, modelo, fonte, variable, significancia) |> 
  rename("series" = "fonte",
         "var_name" = "variable") |> 
  mutate(var_name = if_else(var_name == "CONCESSOES","delta_concessoes_acum", "delta_pgto_acum"),
         modelo = if_else(modelo == "VAR", "var", "vec"))

# ----------------------------------------------------------
# Configuração dos modelos/fontes
# ----------------------------------------------------------

config_efeito <- tribble(
  ~modelo, ~fonte, ~dados,                  ~col_concessoes,      ~col_pgto,
  "vec",   "bcb",  "irf_plots_data_VEC",    "concessoes_bcb",     "pgto_bcb",
  "var",   "bcb",  "irf_plots_data_VAR",    "concessoes_bcb",     "pgto_bcb",
  "vec",   "bru",  "irf_plots_data_VEC",    "concessoes_bru",     "pgto_bru",
  "var",   "bru",  "irf_plots_data_VAR",    "concessoes_bru",     "pgto_bru"
)

calcular_ff <- function(modelo,
                        fonte,
                        dados,
                        col_concessoes,
                        col_pgto) {
  
  df <- get(dados)
  
  df |>
    filter(
      fonte == !!fonte,
      impulse == variavel,
      variable %in% c("CONCESSOES", "PGTO")
    ) |>
    select(modalidade, horizon, variable, Mean) |>
    
    left_join(
      df_nivel_base,
      by = join_by(modalidade == modalidade_agregada)
    ) |>
    
    mutate(
      nivel_base = case_when(
        variable == "CONCESSOES" ~ .data[[col_concessoes]],
        variable == "PGTO"       ~ .data[[col_pgto]]
      ),
      
      delta_R = nivel_base * (exp(Mean) - 1)
    ) |>
    
    select(modalidade, horizon, variable, delta_R) |>
    
    pivot_wider(
      names_from = variable,
      values_from = delta_R,
      names_prefix = "delta_"
    ) |>
    
    group_by(modalidade) |>
    
    summarise(
      delta_concessoes_acum = sum(delta_CONCESSOES, na.rm = TRUE),
      delta_pgto_acum       = sum(delta_PGTO, na.rm = TRUE),
      .groups = "drop"
    ) |>
    
    mutate(
      ff = delta_concessoes_acum - delta_pgto_acum,
      modelo = modelo,
      fonte = fonte
    )
}


df_ff_efeito <- pmap_dfr(
  config_efeito,
  calcular_ff
)


#agrega impactos em formato long
impacto_long <- df_ff_efeito |> 
  select(modalidade, delta_concessoes_acum, delta_pgto_acum, modelo, fonte) |> 
  pivot_longer(cols = c(delta_concessoes_acum, delta_pgto_acum), names_to = "var_name") |> 
  rename(series = fonte)

# ------------------------------------------------------------
# 1. Data used in the plot
# ------------------------------------------------------------

#data para impacto completo
df_plot <- impacto_long %>%
  mutate(value = ifelse(var_name == "delta_pgto_acum", -value, value))

# Total represented by each stacked bar
df_totals <- df_plot %>%
  group_by(modalidade, modelo, series) %>%
  summarise(
    total = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

#data somente para valores significativos

df_plot_sign <- df_plot |> 
  left_join(significancia, by = join_by(modalidade, modelo, series, var_name)) |> 
  filter(significancia == "sim") |> 
  select(-significancia)


# Total represented by each stacked bar
df_totals_sign <- df_plot_sign %>%
  group_by(modalidade, modelo, series) %>%
  summarise(
    total = sum(value, na.rm = TRUE),
    .groups = "drop"
  )


modalidades_unconventional <- c(
  "CC parcelado sem juros",
  "CC parcelado",
  "CC rotativo",
  "Cheque especial"
)

modalidades_conventional <- c(
  "Aquisição de bens",
  "Consignado",
  "Crédito pessoal não consignado",
  "Imobiliário"
)

####MONTA DATAFRAMES PARA GRÁFICOS####

df_plot_conventional_sign <- df_plot_sign %>%
  filter(modalidade %in% modalidades_conventional)

df_plot_unconventional_sign <- df_plot_sign %>%
  filter(modalidade %in% modalidades_unconventional)

df_totals_conventional_sign <- df_totals_sign %>%
  filter(modalidade %in% modalidades_conventional)

df_totals_unconventional_sign <- df_totals_sign %>%
  filter(modalidade %in% modalidades_unconventional)

df_plot_conventional_total <- df_plot %>%
  filter(modalidade %in% modalidades_conventional)

df_plot_unconventional_total <- df_plot %>%
  filter(modalidade %in% modalidades_unconventional)

df_totals_conventional_total <- df_totals %>%
  filter(modalidade %in% modalidades_conventional)

df_totals_unconventional_total <- df_totals %>%
  filter(modalidade %in% modalidades_unconventional)

####FUNÇÃO PARA GERAR PLOT####

plot_impacto <- function(df, df_totals, titulo_grupo) {
  
  ggplot(
    df,
    aes(
      x = interaction(series, modelo),
      y = value
    )
  ) +
    
    # ----------------------------------------------------------
  # Stacked bars
  # ----------------------------------------------------------
  geom_col_pattern(
    aes(
      fill = series,
      pattern = modelo,
      alpha = var_name
    ),
    position = "stack",
    color = "white",
    linewidth = 0.25,
    
    pattern_color = "grey70",
    pattern_alpha = 0.25,
    pattern_density = 0.12,
    pattern_spacing = 0.06
  ) +
    
    # ----------------------------------------------------------
  # Total marker
  # ----------------------------------------------------------
  # geom_point(
  #   data = df_totals,
  #   aes(
  #     x = interaction(series, modelo),
  #     y = total
  #   ),
  #   inherit.aes = FALSE,
  #   shape = 23,
  #   size = 5,
  #   fill = "black",
  #   color = "white",
  #   stroke = 0.5
  # ) +
  geom_point(
    data = df_totals,
    aes(
      x = interaction(series, modelo),
      y = total,
      shape = "Impulso de crédito"
    ),
    inherit.aes = FALSE,
    size = 3,
    fill = "black",
    color = "white",
    stroke = 0.5
  ) +
    scale_shape_manual(
      values = c("Impulso de crédito" = 23),
      name = NULL
    )+  
  facet_wrap(
      ~ modalidade,
      scales = "free_y",
      ncol = 2
    )+

    # ----------------------------------------------------------
  # Source
  # ----------------------------------------------------------
  scale_fill_manual(
    values = c(
      "bcb" = "blue2",
      "bru" = "red3"
    ),
    labels = c(
      "bcb" = "Contábil",
      "bru" = "Estoque"
    ),
    name = "Abordagem"
  ) +
    # Transparency by source # ----------------------------------------------------------
  scale_alpha_manual( 
    values = c( 
      "delta_pgto_acum" = 0.50, 
      "delta_concessoes_acum" = 1.00 ), 
    labels = c( 
      "delta_pgto_acum" = "Pagamentos", 
      "delta_concessoes_acum" = "Concessões" ), 
    name = "var_name" ) +
    # ----------------------------------------------------------
  # Model
  # ----------------------------------------------------------
  scale_pattern_manual(
    values = c(
      "var" = "none",
      "vec" = "stripe"
    ),
    labels = c(
      "var" = "VAR",
      "vec" = "VEC"
    ),
    name = "Modelo"
  ) +
    
    # ----------------------------------------------------------
  # X-axis labels
  # ----------------------------------------------------------
  scale_x_discrete(
    labels = function(x) {
      x |>
        gsub("bcb\\.", "Contábil\n", x = _) |>
        gsub("bru\\.", "Estoque\n", x = _) |>
        gsub("var", "VAR", x = _) |>
        gsub("vec", "VEC", x = _)
    }
  )+
    scale_y_continuous(
      labels = scales::label_comma(
        big.mark = ".", decimal.mark = ",")
    ) +
    labs(
      x = NULL,
      y = "Impulso Mensal (R$ milhões)",
      title = titulo_grupo
      # subtitle = "Bars show the contribution of each variable; diamonds indicate the total"
    ) +
    
    theme_minimal(base_size = 11) +
    
    theme(
      text = element_text(size = 16),       # aumenta todas as fontes
      axis.title = element_text(size = 16), # títulos dos eixos
      axis.text = element_text(size = 16),  # valores dos eixos
      # legend.text = element_text(size = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      panel.grid.major.y = element_line(
        linewidth = 0.25
      ),
      
      strip.background = element_rect(
        fill = "grey92",
        color = NA
      ),
      
      strip.text = element_text(
        face = "bold"
      ),
      
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5
      ),
      
      legend.position = "bottom",
      
      legend.box = "horizontal",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      
      plot.subtitle = element_text(
        color = "grey35"
      )
    )+
    guides(
      
      # ----------------------------------------------------------
      # Source legend: color only, no stripes
      # ----------------------------------------------------------
      fill = guide_legend(
        title = "Abordagem",
        order = 1,
        override.aes = list(
          alpha = 1,
          pattern = "none"
        )
      ),
      
      # ----------------------------------------------------------
      # Model legend: pattern only
      # ----------------------------------------------------------
      pattern = guide_legend(
        title = "Modelo",
        order = 2,
        override.aes = list(
          fill = "grey70",
          alpha = 1,
          pattern = c("none", "stripe"),
          pattern_color = "grey70",
          pattern_alpha = 0.50,
          pattern_density = 0.10,
          pattern_spacing = 0.07
        )
      ),
      
      # ----------------------------------------------------------
      # Variable legend: alpha only
      # ----------------------------------------------------------
      alpha = guide_legend(
        title = "Variável",
        order = 3,
        override.aes = list(
          fill = "grey50",
          pattern = "none",
          alpha = c(1.00, 0.5)
        )
      ),
      shape = guide_legend(
        title = NULL,
        order = 4,
        override.aes = list(
          fill = "black",
          color = "white",
          size = 3
        )
    )
    )
}


####PLOT SOMENTE SIGNIFICATIVOS####

p_conventional <- plot_impacto(
  df_plot_conventional_sign,
  df_totals_conventional_sign,
  "Convencionais"
)+ 
  theme(legend.position = "none")

p_unconventional <- plot_impacto(
  df_plot_unconventional_sign,
  df_totals_unconventional_sign,
  "Não Convencionais"
)+ 
 labs(y = NULL) +
  theme(legend.position = "none")

separator <- ggplot() +
  geom_vline(
    xintercept = 0,
    color = "darkgrey",
    linewidth = 0.6
  ) +
  theme_void()


plot_impulse_sign <- (p_conventional |  separator | p_unconventional) + 
  plot_layout(
    widths = c(1, 0.03, 1),
    guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold")
  )

plot_impulse_sign

ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\06_plot_impulse_sign.svg"),
       plot = plot_impulse_sign, width = 20, height = 10, dpi = 1000)


####PLOT TOTAL, INCLUSIVE NÃO SIGNIFICATIVO####

p_conventional <- plot_impacto(
  df_plot_conventional_total,
  df_totals_conventional_total,
  "Convencionais"
)+ 
  theme(legend.position = "none")

p_unconventional <- plot_impacto(
  df_plot_unconventional_total,
  df_totals_unconventional_total,
  "Não Convencionais"
)+ 
  labs(y = NULL) +
  theme(legend.position = "none")

separator <- ggplot() +
  geom_vline(
    xintercept = 0,
    color = "darkgrey",
    linewidth = 0.6
  ) +
  theme_void()


plot_impulse_total <- (p_conventional |  separator | p_unconventional) + 
  plot_layout(
    widths = c(1, 0.03, 1),
    guides = "collect") & 
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold")
  )

plot_impulse_total

ggsave(filename = paste0(".\\Output\\Plots\\chapter_4_1\\06_plot_impulse_total.svg"),
       plot = plot_impulse_total, width = 20, height = 10, dpi = 1000)
