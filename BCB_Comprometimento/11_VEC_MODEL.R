# OBJETIVO:
# Aplica modelo VEC às séries calculados segundo abordagem Contábil e de Estoques
# Exporta resultados para serem comparados com modelo VAR posteriormente

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

#### LISTA MODALIDADES ####
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
  metodo_series = c("")
  # metodo_series = c("d_")
)

#  para  teste
# # # 
# modalidade_selecionada <- "CC parcelado sem juros"
# fonte_dados <- "bcb"
# metodo_series <- ""

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

# Função para extrair os módulos dos autovalores (raízes) diretamente do objeto vec2var
get_vec_roots <- function(vec_obj) {
  # K: número de variáveis, p: número de lags em nível
  K <- vec_obj$K
  p <- vec_obj$p
  
  # Junta as matrizes A_1, A_2, ..., A_p em um único bloco horizontal
  A_mats <- do.call(cbind, vec_obj$A) 
  
  if (p == 1) {
    companion <- A_mats
  } else {
    # Monta a Companion Matrix para o VAR(p) equivalente do VEC
    identity_block <- diag(K * (p - 1))
    zero_block <- matrix(0, nrow = K * (p - 1), ncol = K)
    companion <- rbind(
      A_mats,
      cbind(identity_block, zero_block)
    )
  }
  
  # Calcula os módulos dos autovalores da matriz companheira
  eigen_vals <- eigen(companion)$values
  mod_roots <- Mod(eigen_vals)
  
  return(sort(mod_roots, decreasing = TRUE))
}


VEC_model_function <- function(modalidade_selecionada, fonte_dados, metodo_series) {
  
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
  
  
  # --- Preparação dos Dados ---
  df_data <- df_data_model |>  
    filter(modalidade_agregada == modalidade_selecionada) |>
    mutate(
      SELIC = .data[[paste0(metodo_series,"SELIC")]],
      PIB = .data[[paste0(metodo_series,"sa_log_PIB")]],
      CONCESSOES = .data[[paste0(metodo_series,"sa_log_concessoes_", fonte_dados)]],
      PGTO = .data[[paste0(metodo_series,"sa_log_pgto_", fonte_dados)]],
      TX_JUROS = if (tem_tx_juros) .data[["TX_JUROS"]] else NA_real_
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
  
  # #TESTE A MUDANÇA ORDENAMENTO VARIÁVEIS
  # if (tem_tx_juros) {
  #   ordem_vars <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO","SELIC")
  # } else {
  #   ordem_vars <- c("PIB", "CONCESSOES", "PGTO", "SELIC")
  # }
  #TESTE B MUDANÇA ORDENAMENTO VARIÁVEIS
  # if (tem_tx_juros) {
  #   ordem_vars <- c("PIB", "CONCESSOES", "PGTO", "SELIC", "TX_JUROS")
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
  k_lag <- min(top_lags)
  
  # Teste de Johansen (ecdet = 'const' para incluir intercepto na relação de cointegração)
  johansen_test <- urca::ca.jo(ts_data_modelo, type = "trace", ecdet = c("const"), K = k_lag , spec = "longrun")
  r_rank <- choose_r(johansen_test, signif = 0.05)
  
  # summary(johansen_test)
  
  message("Posto (r) identificado: ", r_rank)
  p_var <- max(1, k_lag - 1)
  
  # --- Estimação do Modelo ---
  
  vec_model <- vars::vec2var(johansen_test, r = r_rank)
  vec_model_chol <- id.chol(vec_model)
  
  A_hat <- cbind(
    vec_model$deterministic,
    do.call(cbind, vec_model$A)
  )
  vec_model_chol$A_hat <- A_hat
  
  vec_model_boot <- mb.boot(vec_model_chol, design="fixed", b.length = svars_b_lenght, nboot = 1000, n.ahead = 12, nc = 4, signrest = NULL)
  
  # vec_model_boot <- wild.boot(vec_model_chol, design = "fixed", distr = "rademacher",
  #                             n.ahead = 12, nboot = 1000, nc = 4)

  # summary(vec_model)
  lags_multi_por_p <- 2 * k_lag  
  
  
  K <- ncol(ts_data_modelo)
  T_efetivo <- nrow(residuals(vec_model))
  
  # regra de viabilidade: pelo menos ~3 observações por parâmetro da regressão auxiliar
  teto_viavel <- floor(T_efetivo / (3 * K * (K + 1) / 2))
  
  lags_multi_por_p <- min(2 * k_lag, teto_viavel)
  lags_multi_por_p <- max(lags_multi_por_p, 1)  # nunca menos que 1
  
  serial_bg <- vars::serial.test(vec_model, lags.bg = lags_multi_por_p, type = "BG")
  arch_res  <- vars::arch.test(vec_model, lags.multi = lags_multi_por_p, lags.single = lags_multi_por_p, multivariate.only = TRUE)
  # serial_pt  <- vars::serial.test(vec_model, lags.pt = lags_multi_por_p, type = "PT.asymptotic")
  norm_res   <- vars::normality.test(vec_model)
  
  #raízes do VEC
  # 1. Extrai todas as raízes do VEC restrito
  roots_vec <- get_vec_roots(vec_model)
  
  # 2. Em um VEC(r), as primeiras (K - r) raízes DEVEM ser unitárias (= 1.0)
  # As raízes ESTÁVEIS são as restantes: das posições (K - r + 1) até o final
  num_unit_roots <- K - r_rank
  
  if (num_unit_roots > 0) {
    stable_roots <- roots_vec[(num_unit_roots + 1):length(roots_vec)]
  } else {
    stable_roots <- roots_vec
  }
  
  # 3. Autovalor Máximo ESTÁVEL (deve ser < 1)
  valid_roots <- if (length(stable_roots) > 0) max(stable_roots) else NA_real_
  
  # O VEC é estável se o autovalor máximo ESTÁVEL for menor que 1 
  # (ignorando as K-r raízes unitárias teóricas da cointegração)
  stable <- if (!is.na(valid_roots)) (valid_roots < 1.0) else FALSE
  
  
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

    mean_irf <- vec_model_boot[["true"]][["irf"]][[irf_label]]
    boot_irfs <- sapply(vec_model_boot$bootstrap, function(x) x$irf[[irf_label]])
    
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
      mutate(modalidade = modalidade_selecionada
      )
    
    resultados <- irf_data 
    
    # Store results in lista_irf
    lista_irf[[paste(group, collapse = ", ")]] <- irf_data
    }
  
  
  #### IRF DATA####
  df_irf_completo <- do.call(rbind, lista_irf)
  
  #### 2. CALCULAR OS DESVIOS PADRÃO DOS RESÍDUOS ####
  # Captura os resíduos do modelo para calcular os SDs exatos
  residuos <- resid(vec_model)
  choque_desejado <- 0.5
  
  sd_selic <- sd(residuos[, "resids of SELIC"], na.rm = TRUE)
  fator_selic <- choque_desejado / sd_selic
  
  impulsos_selecionados <- "SELIC"
  
  if (tem_tx_juros) {
    sd_juros <- sd(residuos[, "resids of TX_JUROS"], na.rm = TRUE)
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
  
  sd_por_variavel <- resid(vec_model) |> 
    as.data.frame() |> 
    summarise(across(everything(), sd)) |> 
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "sd_residuo")
  
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
    rank = r_rank,
    lag = k_lag,
    stable = stable,
    # serial_pt = serial_pt$serial$p.value,
    serial_bg = serial_bg$serial$p.value,
    arch = arch_res$arch.mul$p.value,
    normality = norm_res$jb.mul$p.value,
    roots = valid_roots,
    sd_residuos = sd_por_variavel
  ))
}



VEC_model_function_safe <- safely(VEC_model_function)

VEC_resultados <- pmap(grid, VEC_model_function_safe) 

write_rds(VEC_resultados, file = ".\\Output\\VEC_resultados.rds")
# VEC_resultados <- read_rds(file = ".\\Output\\VEC_resultados.rds")

outputs <- map(VEC_resultados, "result")
erros   <- map(VEC_resultados, "error")

df_irf_plots_data_VEC_selecionado <- bind_rows(lapply(outputs, `[[`, 1))|> mutate(modelo = "VEC")
df_irf_plots_data_VEC_apendice <- bind_rows(lapply(outputs, `[[`, 2))|> mutate(modelo = "VEC")
df_residuos <- bind_rows(lapply(outputs, `[[`, 3))|> mutate(modelo = "VEC")
df_data <- bind_rows(lapply(outputs, `[[`, 4))|> mutate(modelo = "VEC")

write_rds(df_irf_plots_data_VEC_selecionado, file = ".\\Output\\df_irf_plots_data_VEC_selecionado.rds")
write.csv2(df_irf_plots_data_VEC_selecionado, file = ".\\Output\\df_irf_plots_data_VEC_selecionado.csv")

write_rds(df_irf_plots_data_VEC_apendice, file = ".\\Output\\df_irf_plots_data_VEC_apendice.rds")
write.csv2(df_irf_plots_data_VEC_apendice, file = ".\\Output\\df_irf_plots_data_VEC_apendice.csv")


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

write.csv2(df_sd_residuos, file = ".\\Output\\sd_residuos_VEC.csv")


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


VEC_export_table <- diagnostic |> 
  mutate(Abordagem = if_else(fonte_dados == "bru", "Estoque", "Contábil")) |> 
  select(modalidade, Abordagem, max_root, rank, lag,arch_pvalue, serial_bg_pvalue) |> 
  mutate(max_root = round(max_root, 3),
         arch_pvalue =  round(arch_pvalue, 3),
         serial_bg_pvalue = round(serial_bg_pvalue, 3)) |> 
  rename(Modalidade= modalidade,
         "Autovalor Máximo" = max_root,
         Posto = rank,
         Lag = lag,
         "p-valor ARCH" = arch_pvalue,
         "p-valor Breusch-Godfrey LM" = serial_bg_pvalue) 
VEC_export_table

write.csv2(VEC_export_table, file = ".\\Output\\VEC_export_table.csv")
gt::gtsave(VEC_export_table |> gt::gt(), filename = paste0(".\\Output\\Plots\\chapter_4_1\\VEC_export_table.docx"))



ordem_variaveis <- c("SELIC", "TX_JUROS", "PIB", "CONCESSOES", "PGTO")
# ordem_variaveis <- c("TX_JUROS", "PIB", "CONCESSOES", "PGTO", "SELIC") #TESTE A
# ordem_variaveis <- c("PIB", "CONCESSOES", "PGTO", "SELIC", "TX_JUROS") #TESTE B

# Unir todas IRFs em um único dataframe
irf_plots_data_VEC_selecionado <- df_irf_plots_data_VEC_selecionado |> 
  select("horizon", "impulse", "variable", "Mean", "Low_05", "Low_10", "Low_16", "Up_95", "Up_90", "Up_84", "modalidade", "metodo", "fonte" )|> 
  mutate(variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         modelo = "VEC")

write_rds(irf_plots_data_VEC_selecionado, file = ".\\Output\\irf_plots_data_VEC_selecionado.rds")
write.csv2(irf_plots_data_VEC_selecionado, file = ".\\Output\\irf_plots_data_VEC_selecionado.csv")

irf_plots_data_VEC_apendice <- df_irf_plots_data_VEC_apendice |> 
  select("horizon", "impulse", "variable", "Mean", "Low_05", "Low_10", "Low_16", "Up_95", "Up_90", "Up_84", "modalidade", "metodo", "fonte" )|> 
  mutate(variable = factor(variable, levels = ordem_variaveis),
         impulse = factor(impulse, levels = ordem_variaveis),
         modelo = "VEC")

write_rds(irf_plots_data_VEC_apendice, file = ".\\Output\\irf_plots_data_VEC_apendice.rds")
write.csv2(irf_plots_data_VEC_apendice, file = ".\\Output\\irf_plots_data_VEC_apendice.csv")
