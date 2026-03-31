date_file <- "2026-03-22"
df_data_model <-  readr::read_rds(file = paste0("./Output/df_data_model_TOTAL",  date_file, ".rds")) 


#### LISTA MODALIDADES ####
grid <- expand_grid(

  fonte_dados = c("bru", "bcb"),
  # fonte_dados = c("bru"),
  
  # metodo_series = c("d_", "")
  metodo_series = c("")
  
)

#### FUNÇÃO AUXILIAR PARA ESCOLHER R ####
choose_r <- function(johansen_test, signif = 0.05) {
  
  teststat <- johansen_test@teststat
  teststat <- rev(teststat)
  critvals <- johansen_test@cval[, paste0(100*signif, "pct")]
  critvals <- rev(critvals)
  
  # número de variáveis
  k <- length(teststat)
  
  # encontrar primeiro NÃO-rejeitado
  r <- 0
  for (i in seq_along(teststat)) {
    if (teststat[i] < critvals[i]) {
      r <- i - 1
      break
    }
  }
  
  # se rejeita todos, então r = k
  if (all(teststat > critvals)) {
    r <- k
  }
  
  return(r)
}

# 
# modalidade_selecionada <- "Cheque especial"
# fonte_dados <- "bru"
# metodo_series <- ""


VEC_model_function <- function(fonte_dados, metodo_series) {
  
  metodo_extenso <- dplyr::if_else(metodo_series == "d_", "diff", "nivel")
  
  cat("Rodando: TOTAL", "| Fonte:", fonte_dados, "| Formato:", metodo_extenso, "\n")
  
  # --- Preparação dos Dados ---
  df_data <- df_data_model |>  
    mutate(
      SELIC = .data[[paste0(metodo_series,"SELIC")]],
      TX_JUROS = .data[[paste0(metodo_series,"TX_JUROS")]],
      PIB = .data[[paste0(metodo_series,"sa_log_PIB")]],
      CONCESSOES = .data[[paste0(metodo_series,"sa_log_concessoes_", fonte_dados)]],
      PGTO = .data[[paste0(metodo_series,"sa_log_pgto_", fonte_dados)]]
    ) %>% ungroup() |> arrange(date) 
  
  # Definir ordem causal: Variáveis mais exógenas primeiro
  ordem_vars <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
  
  modelo <- as.data.frame(df_data)
  
  ts_data_modelo <- modelo |> 
    select(all_of(ordem_vars)) |>  # agora seleciona apenas as variáveis numéricas
    as.matrix() |> 
    ts(start = c(year(min(modelo$date)), month(min(modelo$date))), 
       frequency = 12)
  
  
  # --- Seleção de Lag e Teste de Johansen ---
  # K é o lag do VAR em nível. O VECM usará K-1.
  VARselect_out <- vars::VARselect(ts_data_modelo, lag.max = 10, type = "const")
  # k_lag <- as.numeric(VARselect_out$selection["AIC(n)"])
  
  selected_lags <- as.vector(VARselect_out$selection)
  filtered_lags <- selected_lags[selected_lags <= 10]
  
  # Contagem de frequência
  lag_counts <- table(filtered_lags)
  
  # Lags mais frequentes
  top_lags <- as.numeric(names(lag_counts[lag_counts == max(lag_counts)]))
  
  # Regra de decisão
  k_lag <- if (length(top_lags) == 1) {
    top_lags
  } else {
    as.numeric(VARselect_out$selection["AIC(n)"])
  }
  
  # Teste de Johansen (ecdet = 'const' para incluir intercepto na relação de cointegração)
  johansen_test <- urca::ca.jo(ts_data_modelo, type = "trace", ecdet = c("const"), K = k_lag, spec = "longrun")
  r_rank <- choose_r(johansen_test, signif = 0.05)
  
  # summary(johansen_test)
  
  message("Posto (r) identificado: ", r_rank)
  p_var <- max(1, k_lag - 1)
  
  # --- Estimação do Modelo ---
  
  vec_model <- vars::vec2var(johansen_test, r = r_rank)
  
  # summary(vec_model)
  
  # serial_pt  <- vars::serial.test(vec_model, lags.pt = 12, type = "PT.asymptotic")
  serial_bg  <- vars::serial.test(vec_model, lags.bg = 12, type = "BG")
  arch_res   <- vars::arch.test(vec_model, lags.multi = 12)
  norm_res   <- vars::normality.test(vec_model)
  
  var_diag <- vars::VAR(ts_data_modelo, p = p_var, type = "const")
  roots_var <- vars::roots(var_diag)
  stable <- all(roots_var < 1)
  
  # --- Geração de IRFs (Choque na SELIC) ---
  
  grid_irf <- expand.grid(confidence_intervals = c(0.84, 0.90, 0.95),
                          var_impulse = c("SELIC", "TX_JUROS", "PIB", "CONCESSOES","PGTO"),
                          var_response = c("SELIC", "TX_JUROS", "PIB", "CONCESSOES","PGTO"))
  
  vec_irf <- function(var_impulse, var_response, confidence_intervals)
  {vars::irf(vec_model, 
             impulse = var_impulse,
             response = var_response,
             n.ahead = 12, 
             boot = TRUE, 
             runs = 1000, 
             ci = confidence_intervals, 
             ortho = TRUE)}
  
  
  irf_list <- purrr::pmap(grid_irf, vec_irf)
  
  names(irf_list) <- paste0("imp___", grid_irf$var_impulse, "___ci___", grid_irf$confidence_intervals)
  
  
  
  df_plot_irf <- purrr::imap_dfr(irf_list, function(irf_obj, name_ci) {
    
    # Extrair informações do nome
    parts <- strsplit(name_ci, "___")[[1]]
    impulse_var <- parts[2]  # O impulso está na segunda posição
    ci_val <- as.numeric(parts[4])  # O intervalo está na quarta posição
    
    vars_resp <- colnames(irf_obj$irf[[1]])
    
    purrr::map_dfr(vars_resp, function(v) {
      data.frame(
        horizon = 0:12,
        impulse = impulse_var,
        variable = v,
        mean  = irf_obj$irf[[1]][, v],
        lower = irf_obj$Lower[[1]][, v],
        upper = irf_obj$Upper[[1]][, v],
        ci = ci_val             # Adiciona informação do intervalo
      )
    })
  })
  
  df_plot_irf_wide <- df_plot_irf |> 
    pivot_wider(names_from = ci, values_from = c(mean, lower, upper)) |> 
    select(-mean_0.84, -mean_0.9) |> 
    rename(Mean = mean_0.95,
           Low_05 = lower_0.95,
           Low_10 = lower_0.9,
           Low_16 = lower_0.84,
           Up_95 = upper_0.95,
           Up_90 = upper_0.9,
           Up_84 = upper_0.84
    )
  
  choque_desejado <- 0.5 
  sd_selic <- sd(resid(vec_model)[, "resids of SELIC"])
  fator_escala <- choque_desejado / sd_selic
  
  df_plot_irf_05pts <- df_plot_irf_wide |> 
    mutate(across(c(Mean, Low_05, Low_10, Low_16, Up_95, Up_90, Up_84), ~ .x* fator_escala))
  
  #### RESIDUAL DATA####
  res_df <- as.data.frame(residuals(vec_model))
  res_df$time <- seq_len(nrow(res_df))  # fallback to numeric index
  
  #### DATA####
  
  df_data_model_long <- df_data |> 
    tidyr::pivot_longer(cols = all_of(ordem_vars),  
                        names_to = "serie", 
                        values_to = "valor") |> 
    mutate(serie = factor(serie, levels = ordem_vars))
  
  
  
  return(list(
    data_padrao <- tibble::tibble(
      df_plot_irf_05pts|>  
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
    rank = r_rank,
    lag = k_lag,
    stable = stable,
    # serial_pt = serial_pt$serial$p.value,
    serial_bg = serial_bg$serial$p.value,
    arch = arch_res$arch.mul$p.value,
    normality = norm_res$jb.mul$p.value,
    roots = roots_var
  ))
}



VEC_model_function_safe <- safely(VEC_model_function)

VEC_resultados <- pmap(grid, VEC_model_function_safe) #CERCA DE 1 HORA

write_rds(VEC_resultados, file = ".\\Output\\VEC_resultados_TOTAL.rds")
# VEC_resultados <- read_rds(file = ".\\Output\\VEC_resultados.rds")

outputs <- map(VEC_resultados, "result")
erros   <- map(VEC_resultados, "error")

df_irf_plots_data_VEC <- bind_rows(lapply(outputs, `[[`, 1))|> mutate(modelo = "VEC")
df_residuos <- bind_rows(lapply(outputs, `[[`, 2))|> mutate(modelo = "VEC")
df_data <- bind_rows(lapply(outputs, `[[`, 3))|> mutate(modelo = "VEC")

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


VEC_export_table <- diagnostic |> 
  mutate(Abordagem = if_else(fonte_dados == "bru", "Estoque", "Contábil")) |> 
  select(Abordagem, max_root, rank, lag,arch_pvalue, serial_bg_pvalue) |> 
  mutate(max_root = round(max_root, 3),
         serial_bg_pvalue = round(serial_bg_pvalue, 3)) |> 
  rename("Autovalor Máximo" = max_root,
         Posto = rank,
         Lag = lag,
         "p-valor ARCH" = arch_pvalue,
         "p-valor Breusch-Godfrey LM" = serial_bg_pvalue) 
VEC_export_table


# gt::gtsave(VEC_export_table |> gt::gt(), filename = paste0(".\\Output\\Plots\\chapter_4_1\\VEC_export_table.docx"))




ordem_variaveis <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")

# Unir todas IRFs em um único dataframe
irf_plots_data_VEC <- df_irf_plots_data_VEC|> 
  select("horizon", "impulse", "variable", "Mean", "Low_05", "Low_10", "Low_16", "Up_95", "Up_90", "Up_84","metodo", "fonte" )|> 
  mutate(variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         modelo = "VEC")

write_rds(irf_plots_data_VEC, file = ".\\Output\\irf_plots_data_VEC_TOTAL.rds")

write.csv2(irf_plots_data_VEC, file = ".\\Output\\irf_plots_data_VEC_TOTAL.csv")

