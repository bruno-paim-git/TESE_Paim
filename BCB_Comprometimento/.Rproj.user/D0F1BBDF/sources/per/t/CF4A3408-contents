# OBJETIVO:
# Aplica modelo VAR às séries calculados segundo abordagem Contábil e de Estoques
# Exporta resultados para serem comparados com modelo VEC posteriormente

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

df_data_model <-  readr::read_rds(file = paste0("./Output/df_data_model_",  date_file, ".rds")) 

####LISTA MODALIDADES####
grid <- expand_grid(
  modalidade = c(
    "Aquisição de bens"  ,
    "CC parcelado sem juros",
    "CC parcelado",
    "CC rotativo",
    "Cheque especial",
    "Consignado",
    "Crédito pessoal não consignado",
    "Imobiliário"
  ),
  fonte_dados = c("bru", "bcb"),
  # metodo_series = c("d_", "")
  # metodo_series = c("")
  metodo_series = c("d_")
)
# 
# modalidade_selecionada <-    "CC parcelado sem juros"
# fonte_dados <- "bru"
# metodo_series = c("d_")


VAR_model_function <- function(modalidade_selecionada, fonte_dados, metodo_series) {
  
  metodo_extenso <- dplyr::if_else(metodo_series == "d_", "diff", "nivel")
  
  cat(
    "Rodando:", modalidade_selecionada,
    "\nFonte dos Dados:", fonte_dados,
    "\nFormato:", metodo_extenso,
    "\n\n"
  )
  
  df_data <- df_data_model |>  
    filter(modalidade_agregada ==
             modalidade_selecionada
    ) 
  
  tem_tx_juros <-   any(!is.na(df_data[["TX_JUROS"]])) &&
    any(df_data[["TX_JUROS"]] > 0, na.rm = TRUE)
  
  df_data <- df_data_model |>  
    filter(modalidade_agregada ==
             modalidade_selecionada
    ) |>
    mutate(
      SELIC = .data[[paste0(metodo_series,"SELIC")]],
      PIB = .data[[paste0(metodo_series,"sa_log_PIB")]],
      CONCESSOES = .data[[paste0(metodo_series,"sa_log_concessoes_", fonte_dados)]],
      PGTO = .data[[paste0(metodo_series,"sa_log_pgto_", fonte_dados)]],
      TX_JUROS = if (tem_tx_juros) .data[[paste0(metodo_series,"TX_JUROS")]] else NA_real_
    ) |> 
    ungroup() |> 
    arrange(date) 
  

  ##### SELECIONA VARIÁVEIS DOS MODELOS####
  # MODELOS PARA O CC parcelado sem juros não podem incluir a tx de juros#
  # Demais modalidades
  # (date, SELIC, TX_JUROS, PIB, CONCESSOES, PGTO)
  
  vars_base <- c("SELIC", "PIB", "CONCESSOES", "PGTO")
  
  if (tem_tx_juros) {
    vars_modelo <- c("TX_JUROS", vars_base)
  } else {
    vars_modelo <- vars_base
  }
  
  df_data_modelo <- df_data |>
    dplyr::select(all_of(c("date", vars_modelo))) |>
    drop_na()
  
  # ORDENAMENTO DA TESE
  if (tem_tx_juros) {
    ordem_vars <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
  } else {
    ordem_vars <- c("SELIC", "PIB", "CONCESSOES", "PGTO")
  }
  
  # TESTE A MUDANÇA ORDENAMENTO VARIÁVEIS
  # if (tem_tx_juros) {
  #   ordem_vars <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO","SELIC")
  # } else {
  #   ordem_vars <- c("PIB", "CONCESSOES", "PGTO", "SELIC")
  # }
  # TESTE B MUDANÇA ORDENAMENTO VARIÁVEIS
  # if (tem_tx_juros) {
  #   ordem_vars <- c("PIB", "CONCESSOES", "PGTO","SELIC", "TX_JUROS")
  # } else {
  #   ordem_vars <- c("PIB", "CONCESSOES", "PGTO", "SELIC")
  # }
  
  ys <- paste0("y", seq_along(ordem_vars))
  
  # cria um dataframe com todas as combinações possíveis (incluindo ida e volta)
  n_vars <- length(ordem_vars)
  
  grid_completo <- expand.grid(
    origem = 1:n_vars,
    destino = 1:n_vars
  )
  
  variable_groups <- pmap(grid_completo, function(origem, destino) {
    c(ordem_vars[origem], ordem_vars[destino], ys[origem], ys[destino])
  })
  

  # Definir o menor ano e o menor mês corretamente
  min_year <- min(year(df_data_modelo$date))
  min_month <- min(month(df_data_modelo$date[year(df_data_modelo$date) == min_year]))
  
  # Transforma em TS
  ts_data_modelo <- df_data_modelo |> 
    select(all_of(ordem_vars), everything()) |> 
    as.data.frame() |> 
    select(-date) |> 
    ts(start = c(min_year,min_month), frequency = 12)

  
  svars_b_lenght <- round(0.1*nrow(ts_data_modelo),0)
  #b.length: In applied work, however, a typical block length is about 10% of the sample size.
  
  ######MODELO####
  VARselect <- VARselect(ts_data_modelo, lag.max = 10, type = "const", season = NULL)
  
  # Extract recommended lags
  selected_lags <- as.vector(VARselect$selection)
  filtered_lags <- selected_lags[selected_lags <= 10]
  
  # Contagem de frequência
  lag_counts <- table(filtered_lags)
  
  # Lags mais frequentes
  top_lags <- as.numeric(names(lag_counts[lag_counts == max(lag_counts)]))
  
  # Regra de decisão
  
  most_frequent_lag <-  min(top_lags)
  
  var_model <- VAR(ts_data_modelo, type = "const", p = most_frequent_lag, ic = "AIC")
  var_model_chol <- id.chol(var_model)
  var_model_boot <- mb.boot(var_model_chol, design="fixed", b.length = svars_b_lenght, nboot = 1000, n.ahead = 12, nc = 4, signrest = NULL)
  # var_model_boot <- wild.boot(var_model_chol, design = "fixed", distr = "rademacher",
  #                             n.ahead = 12, nboot = 1000, nc = 4)
  
  # Regra para definir lags dos testes
  lags_multi_por_p <- 2 * most_frequent_lag 
  K <- ncol(ts_data_modelo)
  T_efetivo <- nrow(residuals(var_model))
  
  # regra de viabilidade: pelo menos ~3 observações por parâmetro da regressão auxiliar
  teto_viavel <- floor(T_efetivo / (3 * K * (K + 1) / 2))
  
  lags_multi_por_p <- min(2 * most_frequent_lag, teto_viavel)
  lags_multi_por_p <- max(lags_multi_por_p, 1)  # nunca menos que 1
  
  serial_bg <- vars::serial.test(var_model, lags.bg = lags_multi_por_p, type = "BG")
  arch_res  <- vars::arch.test(var_model, lags.multi = lags_multi_por_p, lags.single = lags_multi_por_p, multivariate.only = TRUE)
  # serial_pt  <- vars::serial.test(vec_model, lags.pt = lags_multi_por_p, type = "PT.asymptotic")
  norm_res   <- vars::normality.test(var_model)
  
  roots_var <- vars::roots(var_model)
  stable <- all(roots_var < 1)
  
  # str(var_model_boot)
  lista_irf <- list()
  
  # Loop through each group of variables
  for (group in variable_groups) {
    
    # Extract the variables in the current group
    var1 <- group[1]
    var2 <- group[2]
    var3 <- group[3]
    var4 <- group[4]
    
    irf_plot <- paste0("IRF: ", var1, " - ", var2)
    
    irf_label <- paste0("epsilon[ ", var1, " ] %->% ", var2)

    # Extrai a resposta da réplica pontual (modelo estimado)
    mean_irf_diff <- var_model_boot[["true"]][["irf"]][[irf_label]]
    
    # Extrai a matriz de bootstraps: linhas = horizontes (1..H), colunas = nboot
    boot_irfs_diff <- sapply(var_model_boot$bootstrap, function(x) x$irf[[irf_label]])
    
    # --- ACUMULAÇÃO DOS RESULTADOS (Diferença -> Nível) ---
    # 1. Acumula a IRF pontual/média ao longo dos horizontes (cumsum)
    mean_irf_accum <- cumsum(mean_irf_diff)
    
    # 2. Acumula a IRF para CADA uma das 1000 trajetórias do Moving Block Bootstrap
    boot_irfs_accum <- apply(boot_irfs_diff, 2, cumsum)
    
    # 3. Extrai os quantis das bandas de confiança sobre as trajetórias ACUMULADAS
    low_05 <- apply(boot_irfs_accum, 1, quantile, probs = 0.05)
    low_10 <- apply(boot_irfs_accum, 1, quantile, probs = 0.10)
    low_16 <- apply(boot_irfs_accum, 1, quantile, probs = 0.16)
    
    up_95 <- apply(boot_irfs_accum, 1, quantile, probs = 0.95)
    up_90 <- apply(boot_irfs_accum, 1, quantile, probs = 0.90)
    up_84 <- apply(boot_irfs_accum, 1, quantile, probs = 0.84)
    
    # Monte o dataframe com a resposta acumulada
    irf_data <- data.frame(
      horizon  = 1:length(mean_irf_accum),
      impulse  = var1,
      variable = var2,
      Mean     = mean_irf_accum,
      Low_05   = low_05,
      Low_10   = low_10,
      Low_16   = low_16,
      Up_95    = up_95,
      Up_90    = up_90,
      Up_84    = up_84
    ) |> 
      mutate(modalidade = modalidade_selecionada)
    
    resultados <- irf_data 
    
    # Store results in lista_irf
    lista_irf[[paste(group, collapse = ", ")]] <- irf_data
  }
  
  
  #### IRF DATA####
  # Dados para o apêndice (Choques em 1 SD)
  df_irf_completo <- do.call(rbind, lista_irf)
  
  #### 2. CALCULAR OS DESVIOS PADRÃO DOS RESÍDUOS ####
  # Captura os resíduos do modelo para calcular os SDs exatos
  residuos <- resid(var_model)
  choque_desejado <- 0.5
  
  sd_selic <- sd(residuos[, "SELIC"], na.rm = TRUE)
  fator_selic <- choque_desejado / sd_selic
  
  impulsos_selecionados <- "SELIC"
  
  if (tem_tx_juros) {
    sd_juros <- sd(residuos[, "TX_JUROS"], na.rm = TRUE)
    fator_juros <- choque_desejado / sd_juros
    impulsos_selecionados <- c("SELIC", "TX_JUROS")
  } else {
    
    fator_juros <- 0
    impulsos_selecionados <- "SELIC"
    
  }
  
  df_irf_selecionado <- df_irf_completo |> 
    mutate(across(
      c(Mean, Low_05, Low_10, Low_16, Up_95, Up_90, Up_84), 
      ~ case_when(
        impulse == "SELIC"    ~ .x * fator_selic,
        impulse == "TX_JUROS" ~ .x * fator_juros,
        TRUE                  ~ .x
      )
    )) |> 
    filter(impulse %in% impulsos_selecionados)
  
  sd_por_variavel <- resid(var_model) |> 
    as.data.frame() |> 
    summarise(across(everything(), sd)) |> 
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "sd_residuo")
  
  #### RESIDUAL DATA####
  res_df <- as.data.frame(residuals(var_model))
  res_df$time <- seq_len(nrow(res_df))  # fallback to numeric index
  
  #### DATA####
  
  df_data_model_long <- df_data_modelo |> 
    tidyr::pivot_longer(cols = all_of(ordem_vars),  
                        names_to = "serie", 
                        values_to = "valor") |> 
    mutate(serie = factor(serie, levels = ordem_vars))
  
  
  return(  list(
    df_irf_selecionado <- tibble::tibble(
      df_irf_selecionado |>  
        mutate(
          modalidade = modalidade_selecionada,
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    df_irf_completo <- tibble::tibble(
      df_irf_completo |>  
        mutate(
          modalidade = modalidade_selecionada,
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    residual_data <- tibble::tibble(
      res_df|>  
        mutate(
          modalidade = modalidade_selecionada,
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    df_data <- tibble::tibble(
      df_data_model_long |>  
        mutate(
          modalidade = modalidade_selecionada,
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    metodo = metodo_extenso,
    model = paste0(modalidade_selecionada, "_", fonte_dados),
    lag = most_frequent_lag,
    stable = stable,
    # serial_pt = serial_pt$serial$p.value,
    serial_bg = serial_bg$serial$p.value,
    arch = arch_res$arch.mul$p.value,
    normality = norm_res$jb.mul$p.value,
    roots = roots_var,
    sd_residuos = sd_por_variavel
    ))
}

VAR_model_function_safe <- safely(VAR_model_function)

VAR_resultados <- pmap(grid, VAR_model_function_safe)
write_rds(VAR_resultados, file = ".\\Output\\VAR_resultados.rds")

# VAR_resultados <- readRDS(file = ".\\Output\\VAR_resultados.rds")
outputs <- map(VAR_resultados, "result")
erros   <- map(VAR_resultados, "error")


# cria dataframes para comparar com VEC
df_irf_plots_data_VAR_selecionado <- bind_rows(lapply(outputs, `[[`, 1))|> mutate(modelo = "VAR")
df_irf_plots_data_VAR_apendice <- bind_rows(lapply(outputs, `[[`, 2))|> mutate(modelo = "VAR")
df_residuos <- bind_rows(lapply(outputs, `[[`, 3))|> mutate(modelo = "VAR")
df_data <- bind_rows(lapply(outputs, `[[`, 4))|> mutate(modelo = "VAR")

write_rds(df_irf_plots_data_VAR_selecionado, file = ".\\Output\\df_irf_plots_data_VAR_selecionado.rds")
write.csv2(df_irf_plots_data_VAR_selecionado, file = ".\\Output\\df_irf_plots_data_VAR_selecionado.csv")

write_rds(df_irf_plots_data_VAR_apendice, file = ".\\Output\\df_irf_plots_data_VAR_apendice.rds")
write.csv2(df_irf_plots_data_VAR_apendice, file = ".\\Output\\df_irf_plots_data_VAR_apendice.csv")


df_sd_residuos <- purrr::map2_dfr(
  outputs, 
  seq_along(outputs),
  ~ if (!is.null(.x)) {
    .x$sd_residuos |> 
      mutate(
        modalidade = grid$modalidade[.y],
        fonte      = grid$fonte_dados[.y],
        metodo     = grid$metodo_series[.y]
      )
  }
)

write.csv2(df_sd_residuos, file = ".\\Output\\sd_residuos_VAR.csv")


diagnostic <- grid |>
  mutate(
    # roots e estabilidade
    roots      = map(outputs, ~ .x$roots %||% NA),
    max_root   = map_dbl(outputs, ~ if(!is.null(.x$roots)) max(.x$roots, na.rm = TRUE) else NA_real_),
    stable     = map_lgl(outputs, ~ .x$stable %||% NA),
    
    # rank e lag
    rank       = map_dbl(outputs, ~ .x$rank %||% NA_real_),
    lag        = map_dbl(outputs, ~ .x$lag %||% NA_real_),
    
    # p-values dos testes
    # serial_pt_pvalue = map_dbl(outputs, ~ .x$serial_pt %||% NA_real_),
    serial_bg_pvalue   = map_dbl(outputs, ~ .x$serial_bg %||% NA_real_),
    arch_pvalue        = map_dbl(outputs, ~ .x$arch %||% NA_real_),
    normality_pvalue   = map_dbl(outputs, ~ .x$normality %||% NA_real_)
  )


VAR_export_table <- diagnostic |> 
  mutate(Abordagem = if_else(fonte_dados == "bru", "Estoque", "Contábil")) |> 
  select(modalidade, Abordagem, max_root, lag,arch_pvalue, serial_bg_pvalue) |> 
  mutate(max_root = round(max_root, 3),
         arch_pvalue =  round(arch_pvalue, 3),
         serial_bg_pvalue = round(serial_bg_pvalue, 3)) |> 
  rename(Modalidade= modalidade,
         "Autovalor Máximo" = max_root,
         Lag = lag,
         "p-valor ARCH" = arch_pvalue,
         "p-valor Breusch-Godfrey LM" = serial_bg_pvalue) 
VAR_export_table

write.csv2(VAR_export_table, file = ".\\Output\\VAR_export_table.csv")
gt::gtsave(VAR_export_table |> gt::gt(), filename = paste0("./Output/Plots/chapter_4_1/VAR_export_table.docx"))

ordem_variaveis <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
# ordem_variaveis <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO", "SELIC") #TESTE A
# ordem_variaveis <- c("PIB", "CONCESSOES", "PGTO", "SELIC", "TX_JUROS") #TESTE B


# Unir todas IRFs em um único dataframe
irf_plots_data_VAR_selecionado <- df_irf_plots_data_VAR_selecionado |> 
  select("horizon", "impulse", "variable", "Mean", "Low_05", "Low_10", "Low_16", "Up_95", "Up_90", "Up_84", "modalidade", "metodo", "fonte" )|> 
  mutate(variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         modelo = "VAR")

write_rds(irf_plots_data_VAR_selecionado, file = ".\\Output\\irf_plots_data_VAR_selecionado.rds")
write.csv2(irf_plots_data_VAR_selecionado, file = ".\\Output\\irf_plots_data_VAR_selecionado.csv")

irf_plots_data_VAR_apendice <- df_irf_plots_data_VAR_apendice |> 
  select("horizon", "impulse", "variable", "Mean", "Low_05", "Low_10", "Low_16", "Up_95", "Up_90", "Up_84", "modalidade", "metodo", "fonte" )|> 
  mutate(variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         modelo = "VAR")

write_rds(irf_plots_data_VAR_apendice, file = ".\\Output\\irf_plots_data_VAR_apendice.rds")
write.csv2(irf_plots_data_VAR_apendice, file = ".\\Output\\irf_plots_data_VAR_apendice.csv")


