{
  ####BIBLIOTECA####
  library(sfcr)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(plotly)
  library(purrr)
  library(tidyverse)
  library(ggraph)
  library(webshot)
  library(gt)
  source("./01_tese_equations.R")  
  
  source("./02_tese_auxiliary_functions.R")  
}

grid_base_models <- tibble::tibble(
  nome_modelo = c("BETA03_PHI06", "BETA06_PHI06", "BETA00_PHI06", "BETA03_PHI05", "BETA06_PHI05", "BETA00_PHI05"),
  beta_v = c(0.03, 0.06 ,  0,    0.03, 0.06,  0),
  phi0_v = c(0.06, 0.06,  0.06, 0.05,  0.05,  0.05))

#Este script insere os parâmetros de calibragem do modelo base utilizado nas simulações
#A estrutura construída da seguinte forma:
#1º: atribuir os valores para as variáveis exógenas
#2º: estimar os valores iniciais a partir das equações e das variáveis exógenas
#3º: criar lista com os valores iniciais e aplicar na função sfcr_set
# #4º: cria diversos cenários base
nome_modelo <- "BETA03_PHI06"
beta_v <- 0.03
phi0_v <- 0.06
selected_equations <- eqs_K
period_simulation <- 1000


####BLOCO 1: DADOS####

#PARA DEFINIR PARÂMETROS EXÓGENOS, VALORES INICIAIS E GERAR MODELO BASE PARA AS SIMULAÇÕES
sfcr_scenarios <- function(nome_modelo, beta_v, phi0_v, selected_equations = eqs_K, period_simulation = 1000){
  
  nome_modelo <- nome_modelo
  
  #####1º: ATRIBUIÇÃO DOS VALORES PARA AS VARIÁVEIS EXÓGENAS####
  
  #LIMITE DE CRÉDITO DO TIPO B SOBRE OS SALÁRIOS: Definido pelo autor. Passíveis de Simulação.
  beta_v <- beta_v
  #PARCELA FIXA DO CONSUMO AUTÔNOMO: Definido pelo autor. Passíveis de Simulação.
  phi0_v <- phi0_v
  
  #PROFIT-SHARE: Miebach e Marquetti (2022) para o ano de 2013.
  pi_v = 0.4297 
  #WAGE-SHARE: Miebach e Marquetti (2022) para o ano de 2013.
  w_v = 0.5703 
  
  #PROPENSÃO A CONSUMIR A PARTIR DOS SALÁRIOS: Leite (2015), com base na POF 2008-09, valor para 92% mais pobres.
  cw_v = 0.894
  
  #PROPENSÃO A CONSUMIR A PARTIR DOS LUCROS: Leite (2015), com base na POF 2008-09, valor para 8% mais ricos.
  cr_v = 0.521 
  
  #UTILIZAÇÃO DA CAPACIDADE: Calculado pelo autor a partir da média de 2013-25, com dados da NUCI-FGV.
  un_v = u_v = 0.7822
  
  #RAZÃO CAPITAL-PRODUTO EM PLENA CAPACIDADE: Calculado pelo autor a partir da média de 2013-25, com dados do NUCI-FGV e do IPEA.
  v_v = 1.70
  
  #TAXA DE DEPRECIAÇÃO: Ignorado.
  delta_v = 0
  
  #TAXA DE JUROS REAL EX-ANTE: Calculado pelo autor a partir dos dados de 2013-25, com dados do BCB.
  i_v = 0.05
  
  #SENSIBILIDADE DO INVESTIMENTO A DESVIOS DA UTILIZAÇÃO DA CAPACIDADE: Mandarino, dos Santos e Macedo (2020)
  gamma_v = 0.009 
  
  #SENSIBILIDADE DO CONSUMO AUTÔNOMO À TAXA DE JUROS: Definido pelo autor.
  phi1_v = 0.08
  
  #PROPORÇÃO DE AMORTIZAÇÃO DO TIPO B: Calculado pelo autor a partir dos dados de 2013-25, com dados do BCB.
  r_b_v = 0.79
  #PROPORÇÃO DE AMORTIZAÇÃO DO TIPO Z: Calculado pelo autor a partir dos dados de 2013-25, com dados do BCB.
  r_z_v = 0.04
  
  #COEFICIENTE DE RELAÇÃO COM TAXA BÁSICA (I) DO TIPO B: Calculado pelo autor a partir dos dados de 2013-25, com dados do BCB.
  lambda_b_v = 50
  # lambda_b_v = 7.24
  
  #COEFICIENTE DO CUSTO DO ESTOQUE DO CRÉDITO DO TIPO AUTÔNOMO (TIPO Z): Calculado pelo autor a partir dos dados de 2013-25, com dados do BCB.
  lambda_z_v = 0.65
  # lambda_z_v = 0.46
  
  # i_b_v = 2.248 + lambda_b_v*i_v
  # i_z_v = 0.014 + lambda_z_v*i_v
  
  i_b_v = lambda_b_v*i_v
  i_z_v = lambda_z_v*i_v
  
  
  ####2º: ESTIMAR VALORES INICIAIS A PARTIR DAS EQUAÇÕES ####
  K_v = 100000
  
  gZ_v = phi0_v - phi1_v*i_v
  
  g <- gZ_v
  
  Yfc_v = (K_v/(1+g))/v_v #Yfc em t=
  
  # Y fixado pela capacidade (coerente com estado estacionário: u=un)
  Y_v <- u_v*Yfc_v
  
  # W 
  W_v <- w_v * Y_v
  
  # h 
  h_v <- (g + delta_v) * v_v / un_v
  I_v <- h_v * Y_v
  
  # Aciona função auxiliar para encontrar a relação Lz/Y de estado estacionário, dados os parâmetros
  
  res <- calcular_lzw_y_equilibrio(
    beta_v=beta_v, phi0_v=phi0_v, phi1_v=phi1_v, i_v=i_v, w_v=w_v, pi_v=pi_v,
    cw_v=cw_v, cr_v=cr_v, un_v=un_v, v_v=v_v, delta_v=delta_v,
    r_b_v=r_b_v, r_z_v=r_z_v, i_b_v=i_b_v, i_z_v=i_z_v
  )

  lz_y = res$lz_y
  
  # Estoque do primeiro período
  Lz_v <- lz_y*Y_v
  Lb_v <-  beta_v * W_v
  L_v  <- Lb_v + Lz_v
  
  # Ajuste variáveis defasadas no primeiro período
  Lz0_v <- Lz_v / (1+g)
  Lb0_v <- Lb_v / (1+g)
  W0_v <- W_v / (1+g)
  
  icc_z_v = i_z_v
  
  icc_b_v =  i_b_v
  
  J_v <- icc_b_v * Lb0_v + icc_z_v * Lz0_v
  A_v <- r_b_v*Lb0_v + r_z_v*Lz0_v
  
  
  Ydw_v <- W_v - J_v
  Ydw_linha_v <- Ydw_v - A_v
  
  Bw_v  <- r_b_v*Lb0_v + beta_v*(W_v - W0_v)   # mesma lógica de steady-growth só para Bw
  Ciw_v <- cw_v * Ydw_linha_v
  
  Fb_v   <- J_v
  FT_v <- pi_v * Y_v
  FD_v = FT_v
  Ydr_v  <- FD_v + Fb_v
  Cr_v   <- cr_v*Ydr_v
  E_v = I_v
  
  # Zw_v inicial é resolvido a partir da identidade Y = C + I
  Zw_v <- Y_v - Ciw_v - Bw_v - Cr_v - I_v
  
  Cw_v  <- Ciw_v + Bw_v + Zw_v
  C_v = Cw_v + Cr_v
  
  # # Checagem: bater EXATAMENTE com Y_v a partir da identidade e a partir de u sobre Yfc
  Ycalc_v <- Cw_v + Cr_v + I_v
  cat("Y_v:", Y_v, " | Ycalc_v:", Ycalc_v, " | diferença:", Y_v - Ycalc_v, "\n")
 
  SAVw_v = Ydw_v - Cw_v
  SAVr_v = Ydr_v - Cr_v
  SAV_v = SAVw_v + SAVr_v
  
  Dw_v = SAVw_v + Bw_v + Zw_v - A_v
     
  Dr_v = SAVr_v - (E_v - E_v/(1+g))
  D_v = Dw_v + Dr_v
  
  # Riqueza líquida inicial
  Vw_v = Dw_v - L_v
  Vr_v = Dr_v + E_v
  Vf_v = K_v - E_v 
  
  ####3º: VALORES INICIAIS E sfcr_set####
  
  #####DEFINIÇÃO: VALORES INICIAIS####
  parametros_iniciais <- list(
    K = K_v,
    Yfc = Yfc_v,
    Y = Ycalc_v,
    
    W = W_v,
    J = J_v,
    A = A_v,
    Ciw = Ciw_v,
    Bw = Bw_v,
    Zw = Zw_v,
    Cw = Cw_v,
    Cr = Cr_v,
    C = C_v,
    
    Dw = Dw_v,
    Dr = Dr_v,
    D = D_v,
    
    u = u_v,
    h = h_v,

    Lb = Lb_v,
    Lz = Lz_v,
    L = L_v,
    
    Ydw = Ydw_v,
    Ydw_linha = Ydw_linha_v,
    Ydr = Ydr_v,
    
    SAVw = SAVw_v,
    SAVr = SAVr_v,
    SAV = SAV_v,
    
    Fb = Fb_v,
    FT = FT_v,
    FD = FD_v,
    E = E_v,
    I = h_v*Y_v,
    
    i_b = i_b_v,
    i_z = i_z_v,
    icc_b = icc_b_v,
    icc_z = icc_z_v,
    
    gZ = gZ_v,
    
    i = i_v,
    pi = pi_v,
    w = w_v,
    cw = cw_v,
    cr = cr_v,
    un = un_v,
    v = v_v,
    delta = delta_v,
    r_b = r_b_v,
    r_z = r_z_v,
    lambda_b = lambda_b_v,
    lambda_z = lambda_z_v,
    gamma = gamma_v,
    phi0 = phi0_v,
    phi1 = phi1_v,
    beta = beta_v
  )
  
  # Transforma em lista de fórmulas
  lista_formula_iniciais <- lapply(names(parametros_iniciais), function(nome) {
    as.formula(paste0(nome, " ~ ", parametros_iniciais[[nome]]))
  })
  
  #####sfcr_set####
  
  # Chama sfcr_set via do.call
  init <- do.call(sfcr_set, lista_formula_iniciais)
  
  #####DEFINIÇÃO: VALORES EXTERNOS####
  
  # Lista nomeada de parâmetros
  parametros_externos <- list(
    pi = pi_v,
    w = w_v,
    cw = cw_v,
    cr = cr_v,
    un = un_v,
    v = v_v,
    delta = delta_v,
    i = i_v,
    gamma = gamma_v,
    phi0 = phi0_v,
    phi1 = phi1_v,
    r_b = r_b_v,
    lambda_b = lambda_b_v,
    r_z = r_z_v,
    lambda_z = lambda_z_v,
    beta = beta_v
  )
  
  # Transforma em lista de fórmulas
  lista_formula_externos <- lapply(names(parametros_externos), function(nome) {
    as.formula(paste0(nome, " ~ ", parametros_externos[[nome]]))
  })
  
  #####sfcr_set####
  # Chama sfcr_set via do.call
  ext <- do.call(sfcr_set, lista_formula_externos)
  
  ####4º: CENÁRIO BASE####
  
  print(nome_modelo)
  sim_model <- sfcr_baseline(
    equations = selected_equations,
    external  = ext,
    initial   = init,
    periods   = period_simulation,
    hidden = c("delta_L" = "delta_D"),
    rhtol = TRUE,
    .hidden_tol = 1e-6,
    

  )
  
  
  df_simulation <- sim_model |>
    mutate(
      modelo = nome_modelo
    )
  
  return(df_simulation
         # sim_model = sim_model
  )
  
}


  
#Consolida resultados 

lista_modelos_baseline <- purrr::pmap(grid_base_models, sfcr_scenarios)
names(lista_modelos_baseline) <- grid_base_models$nome_modelo


modelos_base_resultados_empilhados <- dplyr::bind_rows(lista_modelos_baseline, .id = "modelo" )

modelos_processados <- process_model_df(modelos_base_resultados_empilhados)

negativos <- modelos_base_resultados_empilhados |> filter(Ydw_linha < 0)
table(negativos$modelo)

write.csv(modelos_processados,
          file = file.path("Output", paste0("modelos_base_resultados_empilhados.csv")))


tabela_parametros_iniciais <- tibble(
  Parametro = names(parametros_iniciais),
  Valor = parametros_iniciais
) %>%
  unnest(Valor)

write.csv2(tabela_parametros_iniciais, file = ".\\Output\\tabela_parametros_iniciais.csv")

gt_tabela_iniciais <- tabela_parametros_iniciais %>%
  gt() %>%
  # Formatação para valores menores que 1000 (3 casas decimais)
  fmt_number(
    columns = Valor,
    rows = abs(Valor) < 1000,
    decimals = 3,
    dec_mark = ",",
    sep_mark = "."
  ) %>%
  # Formatação para valores maiores ou iguais a 1000 (2 casas decimais)
  fmt_number(
    columns = Valor,
    rows = abs(Valor) >= 1000,
    decimals = 0,
    dec_mark = ",",
    sep_mark = "."
  )
gt::gtsave(gt_tabela_iniciais, filename = paste0(".\\Output\\tabela_parametros_iniciais.docx"))




tabela_parametros_externos <- tibble(
  Parametro = names(parametros_externos),
  Valor = parametros_externos
) %>%
  unnest(Valor)

write.csv2(parametros_externos, file = ".\\Output\\tabela_parametros_externos.csv")

gt_tabela_externos <- tabela_parametros_externos %>%
  gt() %>%
  # Formatação para valores menores que 1000 (3 casas decimais)
  fmt_number(
    columns = Valor,
    rows = abs(Valor) < 1000,
    decimals = 3,
    dec_mark = ",",
    sep_mark = "."
  ) 
gt::gtsave(gt_tabela_externos, filename = paste0(".\\Output\\tabela_parametros_externos.docx"))


