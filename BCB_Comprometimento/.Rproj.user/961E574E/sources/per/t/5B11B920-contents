date_file <- "2026-03-30"

df_data_model <-  readr::read_rds(file = paste0("./Output/df_data_model_",  date_file, ".rds")) |> 
  select("date", "modalidade_agregada", 
         "SELIC", 
         "TX_JUROS",
         "sa_log_PIB",
         "sa_log_concessoes_bru", "sa_log_concessoes_bcb", 
         "sa_log_pgto_bru", "sa_log_pgto_bcb") 



signif_stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

test_unit_root <- function(x, diff_level = 0) {
  
  x <- na.omit(x)
  
  if(length(x) < 15) return(NULL)
  
  # Escolha da especificação
  type_adf <- if(diff_level == 0) "trend" else "drift"
  
  # ADF (com seleção de lag por AIC)
  adf_obj <- ur.df(x, type = type_adf, selectlags = "AIC")
  
  adf_stat <- adf_obj@teststat[1]
  adf_crit <- adf_obj@cval[1,]
  
  # p-value aproximado (não vem direto no urca)
  adf_p <- approx(
    x = adf_crit,
    y = c(0.01, 0.05, 0.10),
    xout = adf_stat,
    rule = 2
  )$y
  
  # PP test
  pp <- tryCatch(pp.test(x), error = function(e) NULL)
  pp_p <- if(!is.null(pp)) pp$p.value else NA
  
  # KPSS
  kpss <- tryCatch(ur.kpss(x), error = function(e) NULL)
  kpss_stat <- if(!is.null(kpss)) kpss@teststat else NA
  
  tibble(
    adf_stat = adf_stat,
    adf_p = adf_p,
    adf_sig = signif_stars(adf_p),
    pp_p = pp_p,
    pp_sig = signif_stars(pp_p),
    kpss_stat = kpss_stat
  )
}


get_stationarity_full <- function(x, max_diff = 2) {
  
  results <- list()
  
  for (d in 0:max_diff) {
    
    x_test <- if (d == 0) x else diff(x, differences = d)
    
    res <- test_unit_root(x_test, diff_level = d)
    
    if (is.null(res)) next
    
    res$diff <- d
    
    results[[d+1]] <- res
  }
  
  bind_rows(results)
}


variaveis <- c("SELIC", "TX_JUROS", "sa_log_PIB", "sa_log_concessoes_bru", "sa_log_concessoes_bcb", "sa_log_pgto_bru", "sa_log_pgto_bcb")

resultado_testes <- df_data_model %>%
  group_by(modalidade_agregada) %>%
  group_split() %>%
  purrr::map_df(function(df_mod) {
    
    modalidade <- unique(df_mod$modalidade_agregada)
    
    purrr::map_df(variaveis, function(var) {
      
      serie <- log(df_mod[[var]])
      
      res <- get_stationarity_full(serie)
      
      res %>%
        mutate(
          modalidade = modalidade,
          variavel = var
        )
    })
  })


resultado_testes %>%
  select(modalidade, variavel, diff, adf_stat, adf_p, adf_sig, pp_p, pp_sig) %>%
  arrange(modalidade, variavel, diff)
table(resultado_testes$modalidade)

resultado_adf_pp_geral <- resultado_testes |> filter(modalidade == "Consignado" & variavel %in% c("SELIC", "sa_log_PIB")) |> 
  mutate(modalidade = "Geral") |> 
  filter(diff %in% c(0, 1)) |> 
  mutate(adf = paste0(round(adf_stat,3), adf_sig),
         pp = paste0(round(pp_p,3), pp_sig))|> 
  select(-kpss_stat, -adf_stat, -adf_p, -adf_sig, -pp_p, -pp_sig) |> 
  pivot_wider(names_from = diff, values_from = c(adf, pp))

resultado_adf_pp_modalidades <- resultado_testes |> filter(modalidade != "Outros") |> 
filter(!variavel %in% c("SELIC", "sa_log_PIB")) |> 
filter(diff %in% c(0, 1)) |> 

  mutate(adf = paste0(round(adf_stat,3), adf_sig),
         pp = paste0(round(pp_p,3), pp_sig))|> 
  select(-kpss_stat, -adf_stat, -adf_p, -adf_sig, -pp_p, -pp_sig) |> 
  pivot_wider(names_from = diff, values_from = c(adf, pp)) |>
  filter(!(modalidade %in% c("Aquisição de bens", "Consignado", "Crédito pessoal não consignado", "Imobiliário") &
           variavel == "sa_log_concessoes_bru"))


tabela_final_adf_pp <- bind_rows(resultado_adf_pp_geral,resultado_adf_pp_modalidades) |> 
  mutate( variavel = case_when(
    variavel == "sa_log_PIB" ~ "PIB",
    variavel ==                     "sa_log_concessoes_bru" & modalidade %in% c("Cartão de Crédito", "Cheque especial") ~ "Concessões (Estoque)",
    variavel ==                     "sa_log_concessoes_bcb" & modalidade %in% c("Cartão de Crédito", "Cheque especial") ~ "Concessões (Contábil)",
    variavel ==                   "sa_log_concessoes_bcb" ~ "Concessões",
    variavel ==                   "sa_log_pgto_bru" ~ "Pagamento (Estoque)",
    variavel ==                  "sa_log_pgto_bcb" ~ "Pagamento (Contábil)",
    variavel ==                  "TX_JUROS" ~ "Taxa de Juros",
    TRUE ~ variavel
  )) 



gt::gtsave(tabela_final_adf_pp |> gt::gt(), filename = paste0(".\\Output\\Plots\\chapter_4_1\\tabela_final_adf_pp.docx"))
