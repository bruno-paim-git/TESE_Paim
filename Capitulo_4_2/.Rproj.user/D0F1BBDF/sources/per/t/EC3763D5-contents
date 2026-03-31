

grid_base_models <- tibble::tibble(
  nome_modelo = c("BETA135_PHI06", "BETA5_PHI06", "BETA0_PHI06", "BETA135_PHI05", "BETA5_PHI05", "BETA0_PHI05"), 
  beta_v = c(0.135, 0.5,   0,    0.135, 0.5,   0), 
  phi0_v = c(0.06,  0.06,  0.06, 0.05,  0.05,  0.05))

#Este script insere os parâmetros de calibragem do modelo base utilizado nas simulações
#A estrutura construída da seguinte forma:
#1º: atribuir os valores para as variáveis exógenas
#2º: estimar os valores iniciais a partir das equações e das variáveis exógenas
#3º: criar lista com os valores iniciais e aplicar na função sfcr_set
#4º: cria diversos cenários base
# nome_modelo <- "BETA135_PHI06"
# beta_v <- 0.135
# phi0_v <- 0.06
# selected_equations <- eqs_K

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
    
    #PROPENSÃO A CONSUMIR A PARTIR DOS SALÁRIOS:Leite (2015), com base na POF 2008-09, valor para 92% mais pobres.
    cw_v = 0.894 
    #PROPENSÃO A CONSUMIR A PARTIR DOS LUCROS: Leite (2015), com base na POF 2008-09, valor para 8% mais ricos.
    cr_v = 0.521 
    
    #PROPORÇÃO DOS LUCROS DISTRIBUÍDA: Definido pelo autor.
    ro_v = 1 
    
    #UTILIZAÇÃO DA CAPACIDADE: Calculado pelo autor a partir da média de 2013-24, com dados da NUCI-FGV.
    un_v = u_v = 0.7896 
    #RAZÃO CAPITAL-PRODUTO: Calculado pelo autor a partir da média de 2013-24, com dados da NUCI-FGV e do IPEA.
    v_v = 1.82 
    #TAXA DE DEPRECIAÇÃO: Calculado pelo autor a partir da média de 2013-24, com dados do IPEA.
    delta_v = 0.0635452171269733
    
    #TAXA DE JUROS: Calculado pelo autor a partir dos dados de 2013, com dados do BCB.
    i_v = 0.0818 
    #SENSIBILIDADE DO INVESTIMENTO A DESVIOS DA UTILIZAÇÃO DA CAPACIDADE: Mandarino, dos Santos e Macedo (2020)
    gamma_v = 0.009 
    
    #SENSIBILIDADE DO CONSUMO AUTÔNOMO À TAXA DE JUROS: Definido pelo autor.
    phi1_v = 0.08 
    
    
    #PROPORÇÃO DE AMORTIZAÇÃO DO TIPO B: Calculado pelo autor a partir dos dados de 2013, com dados do BCB.
    r_b_v = 0.0358 
    #PROPORÇÃO DE AMORTIZAÇÃO DO TIPO Z: Calculado pelo autor a partir dos dados de 2013, com dados do BCB.
    r_z_v = 0.0125 
    
    #COEFICIENTE DE RELAÇÃO COM TAXA BÁSICA (I) DO TIPO B: Definido pelo autor.
    lambda_b_v = 2
    #COEFICIENTE DE RELAÇÃO COM TAXA BÁSICA (I) DO TIPO Z: Definido pelo autor.
    lambda_z_v = 1
    
    ####2º: ESTIMAR VALORES INICIAIS A PARTIR DAS EQUAÇÕES ####
    K_v = 100000
    h_v = 0.2573517
    
    Dw_v = 0
    Dr_v = 0
    D_v = 0
    
    gZ_v = phi0_v - phi1_v*i_v
    
    Yfc_v = K_v/v_v
    Y_v = Yfc_v*u_v
    Zw_v = Y_v*gZ_v
    
    W_v = w_v*Y_v
    
    Lbw_v = beta_v*W_v
    Lzw_v = 1.428251282*Lbw_v
    
    # Lbw_v = 0.047*W_v
    # Lzw_v = 0.67*W_v
    
    Lw_v = Lzw_v + Lbw_v
    
    FT_v = pi_v*Y_v
    FD_v = ro_v*FT_v
    Fb_v = i_v*Lw_v
    Cr_v = cr_v*(FD_v + Fb_v)
    
    Ydw_v = W_v - (r_b_v+i_v*lambda_b_v)*Lbw_v - (r_z_v+i_v*lambda_z_v)*Lzw_v      
    Ciw_v = cw_v * Ydw_v
    
    Bw_v = r_b_v*Lbw_v + beta_v*W_v*gZ_v
    
    Cw_v = Ciw_v + Bw_v  + Zw_v                          
    
    FU_v = 0
    I_v = h_v*Y_v -delta_v*K_v
    
    E_v = I_v
    Ydr_v = FD_v + Fb_v
    C_v = Cw_v + Cr_v
    Ycalc_v = Cw_v + Cr_v + I_v
    
    SAVw_v = Ydw_v - Cw_v 
    SAVr_v = FD_v + Fb_v - Cr_v 
    SAV_v = SAVw_v + SAVr_v + FU_v
    
    Vr_v = SAVr_v + E_v
    
    ####3º: VALORES INICIAIS E sfcr_set####
    
    
    #####DEFINIÇÃO: VALORES INICIAIS####
    parametros_iniciais <- list(
      W = W_v,
      Yfc = Yfc_v,
      Ciw = Ciw_v,
      Bw = Bw_v,
      Zw = Zw_v,
      Cw = Cw_v,
      Cr = Cr_v,
      C = C_v,
      
      E = E_v,
      # Vr = Vr_v,
      u = u_v,
      h = h_v,
      
      # Dw = Dw_v,
      # Dr = Dr_v,
      # D = D_v,
      
      Lbw = Lbw_v,
      Lzw = Lzw_v,
      Lw = Lbw_v+Lzw_v,
      
      Y = Ycalc_v,
      Ydr = Ydr_v,
      Ydw = Ydw_v,
      
      SAVw = SAVw_v,
      SAVr = SAVr_v,
      SAV = SAV_v,
      
      Fb = Fb_v,
      FT = FT_v,
      FU = FU_v,
      FD = FD_v,
      
      K = K_v,
      I = h_v*Y_v,
      
      gZ = gZ_v
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
      ro = ro_v,
      un = un_v,
      v = v_v,
      delta = delta_v,
      i = i_v,
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
      periods   = period_simulation
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

