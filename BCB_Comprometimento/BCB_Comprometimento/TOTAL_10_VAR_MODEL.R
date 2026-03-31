date_file <- "2026-03-22"
df_data_model <-  readr::read_rds(file = paste0("./Output/df_data_model_TOTAL",  date_file, ".rds")) 


####LISTA MODALIDADES####
grid <- expand_grid(
  fonte_dados = c("bru", "bcb"),
  # metodo_series = c("d_", "")
  metodo_series = c("")
)
# modalidade_selecionada <- "Cartão de Crédito"
# fonte_dados <- "bru"
# metodo_series <- ""



VAR_model_function <- function( fonte_dados, metodo_series) {
  
  metodo_extenso <- dplyr::if_else(metodo_series == "d_", "diff", "nivel")
  
  cat(
    "Rodando: TOTAL",
    "\nFonte dos Dados:", fonte_dados,
    "\nFormato:", metodo_extenso,
    "\n\n"
  )
  
  
  df_data <- df_data_model |>  

    ##variáveis em diferença
    mutate(
      SELIC = .data[[paste0(metodo_series,"SELIC")]],
      TX_JUROS = .data[[paste0(metodo_series,"TX_JUROS")]],
      PIB = .data[[paste0(metodo_series,"sa_log_PIB")]],
      CONCESSOES = .data[[paste0(metodo_series,"sa_log_concessoes_", fonte_dados)]],
      PGTO = .data[[paste0(metodo_series,"sa_log_pgto_", fonte_dados)]]
    )%>%
    ungroup() |> 
    arrange(date) 
  
  
  selected_irf_plots <- list()
  
  ##### SELECIONA VARIÁVEIS DOS MODELOS####
  #MODELOS PARA O CC parcelado sem juros não podem incluir a tx de juros#  
  # Demais modalidades
  # (date, SELIC, TX_JUROS, PIB, CONCESSOES, PGTO)
  
  df_data_modelo <- df_data |>
    dplyr::select(date, SELIC, TX_JUROS, PIB, CONCESSOES, PGTO) |>
    drop_na()
  
  
  # ordem_vars <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
  ordem_vars <- c( "TX_JUROS", "PIB", "CONCESSOES", "PGTO","SELIC")
  
  ys <- c("y1", "y2", "y3", "y4", "y5")
  
  # cria uma matriz com todas as combinações de 2 elementos (sem repetição e sem ordem)
  combinacoes <- combn(length(ordem_vars), 2)
  
  # cria um dataframe com todas as combinações possíveis (incluindo ida e volta)
  grid_completo <- expand.grid(origem = 1:5, destino = 1:5) 
  
  variable_groups <- pmap(grid_completo, function(origem, destino) {
    c(ordem_vars[origem], ordem_vars[destino], ys[origem], ys[destino])
  })
  
  # Obtém a ordem das colunas do data frame, excluindo "date"
  ordem_series <- setdiff(colnames(df_data_modelo), "date")
  
  # Definir o menor ano e o menor mês corretamente
  min_year <- min(year(df_data_modelo$date))
  min_month <- min(month(df_data_modelo$date[year(df_data_modelo$date) == min_year]))
  
  # Transforma em TS
  ts_data_modelo <- df_data_modelo |> 
    as.data.frame() |> 
    select(-date) |> 
    ts(start = c(min_year,min_month), frequency = 12)
  
  svars_b_lenght <- round(0.1*nrow(ts_data_modelo),0)
  
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
  most_frequent_lag <- if (length(top_lags) == 1) {
    top_lags
  } else {
    as.numeric(VARselect$selection["AIC(n)"])
  }
  
  var_model <- VAR(ts_data_modelo, type = "const", p = most_frequent_lag, ic = "AIC")
  var_model_chol <- id.chol(var_model)
  var_model_boot <- mb.boot(var_model_chol, design="fixed", b.length = svars_b_lenght, nboot = 1000, n.ahead = 12, nc = 4, signrest = NULL)
  
  
  # serial_pt  <- vars::serial.test(vec_model, lags.pt = 12, type = "PT.asymptotic")
  serial_bg  <- vars::serial.test(var_model, lags.bg = 12, type = "BG")
  arch_res   <- vars::arch.test(var_model, lags.multi = 12)
  norm_res   <- vars::normality.test(var_model)
  
  roots_var <- vars::roots(var_model)
  stable <- all(roots_var < 1)
  
  #b.length: In applied work, however, a typical block length is about 10% of the sample size.
  
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
    irf_label_2 <- paste0("epsilon[ ", var3, " ] %->% ", var4) #when using recursive design in mb.boot
    
    mean_irf <- var_model_boot[["true"]][["irf"]][[irf_label]]
    boot_irfs <- sapply(var_model_boot$bootstrap, function(x) x$irf[[irf_label]])
    
    # Lower bounds (5%, 10%, 16%)
    low_05 <- apply(boot_irfs, 1, quantile, probs = 0.05)
    low_10 <- apply(boot_irfs, 1, quantile, probs = 0.10)
    low_16 <- apply(boot_irfs, 1, quantile, probs = 0.16)
    
    # Upper bounds (95%, 90%, 84%)
    up_95 <- apply(boot_irfs, 1, quantile, probs = 0.95)
    up_90 <- apply(boot_irfs, 1, quantile, probs = 0.90)
    up_84 <- apply(boot_irfs, 1, quantile, probs = 0.84)
    
    irf_data <- data.frame(
      horizon = 1:length(mean_irf),
      impulse = var1,
      variable = var2,
      Mean = mean_irf,
      Low_05 = low_05,
      Low_10 = low_10,
      Low_16 = low_16,
      Up_95 = up_95,
      Up_90 = up_90,
      Up_84 = up_84
    ) |> 
      mutate(modalidade = "TOTAL",
             plot = irf_plot
      )
    
    resultados <- irf_data 
    
    # Store results in lista_irf
    lista_irf[[paste(group, collapse = ", ")]] <- irf_data
    
    
  }
  
  
  #### IRF DATA####
  df_irf_completo <- do.call(rbind, lista_irf)
  
  #### IRF FOR 0.5pts SELIC CHANGE####
  choque_desejado <- 0.5 
  sd_selic <- sd(resid(var_model)[, "SELIC"])
  fator_escala <- choque_desejado / sd_selic
  
  df_plot_irf_05pts <- df_irf_completo |> 
    mutate(across(c(Mean, Low_05, Low_10, Low_16, Up_95, Up_90, Up_84), ~ .x* fator_escala))
  
  #### RESIDUAL DATA####
  res_df <- as.data.frame(residuals(var_model))
  res_df$time <- seq_len(nrow(res_df))  # fallback to numeric index
  
  #### DATA####
  
  df_data_model_long <- df_data_modelo |> 
    tidyr::pivot_longer(cols = all_of(ordem_series),  
                        names_to = "serie", 
                        values_to = "valor") |> 
    mutate(serie = factor(serie, levels = ordem_series))
  
  
  return(  list(
    VAR_df_irf_completo <- tibble::tibble(
      df_plot_irf_05pts |>  
        mutate(
          modalidade = "TOTAL",
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    residual_data <- tibble::tibble(
      res_df|>  
        mutate(
          modalidade = "TOTAL",
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    df_data <- tibble::tibble(
      df_data_model_long |>  
        mutate(
          modalidade = "TOTAL",
          metodo = metodo_extenso,
          fonte = fonte_dados)),
    metodo = metodo_extenso,
    model = paste0("TOTAL", "_", fonte_dados),
    lag = most_frequent_lag,
    stable = stable,
    # serial_pt = serial_pt$serial$p.value,
    serial_bg = serial_bg$serial$p.value,
    arch = arch_res$arch.mul$p.value,
    normality = norm_res$jb.mul$p.value,
    roots = roots_var
  ))
}

VAR_model_function_safe <- safely(VAR_model_function)

VAR_resultados <- pmap(grid, VAR_model_function_safe)
outputs <- map(VAR_resultados, "result")
erros   <- map(VAR_resultados, "error")

write_rds(VAR_resultados, file = ".\\Output\\VAR_resultados_TOTAL.rds")

df_irf_plots_data_VAR <- bind_rows(lapply(outputs, `[[`, 1))|> mutate(modelo = "VAR")
df_residuos <- bind_rows(lapply(outputs, `[[`, 2))|> mutate(modelo = "VAR")
df_data <- bind_rows(lapply(outputs, `[[`, 3))|> mutate(modelo = "VAR")

write_rds(df_irf_plots_data_VAR, file = ".\\Output\\irf_plots_data_VAR_TOTAL.rds")
write.csv2(df_irf_plots_data_VAR, file = ".\\Output\\irf_plots_data_VAR_TOTAL.csv")

# write_rds(df_irf_plots_data_VAR, file = ".\\Output\\irf_plots_data_VAR_2.rds")
# write.csv2(df_irf_plots_data_VAR, file = ".\\Output\\irf_plots_data_VAR_2.csv")




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
    serial_pt_pvalue = map_dbl(outputs, ~ .x$serial_pt %||% NA_real_),
    serial_bg_pvalue   = map_dbl(outputs, ~ .x$serial_bg %||% NA_real_),
    arch_pvalue        = map_dbl(outputs, ~ .x$arch %||% NA_real_),
    normality_pvalue   = map_dbl(outputs, ~ .x$normality %||% NA_real_)
  )


VAR_export_table <- diagnostic |> 
  mutate(Abordagem = if_else(fonte_dados == "bru", "Estoque", "Contábil")) |> 
  select(Abordagem, max_root, lag,arch_pvalue, serial_bg_pvalue) |> 
  mutate(max_root = round(max_root, 3),
         serial_bg_pvalue = round(serial_bg_pvalue, 3)) |> 
  rename("Autovalor Máximo" = max_root,
         Lag = lag,
         "p-valor ARCH" = arch_pvalue,
         "p-valor Breusch-Godfrey LM" = serial_bg_pvalue) 
VAR_export_table


# gt::gtsave(VAR_export_table |> gt::gt(), filename = paste0(".\\Output\\Plots\\chapter_4_1\\VAR_export_table.docx"))

