# nome_modelo <- "BUCCHIANICO_2024_BASELINE"

#Exercício para reproduzir com o pacote sfcr o artigo:
#DI BUCCHIANICO, Stefano; GALLO, Ettore; LOFARO, Antonino.
#Debt-credit Flows and Stocks in a Supermultiplier Model with Two Autonomous Demand Components: Consequences for Growth.
#Review of Political Economy, [s. l.], v. 36, n. 5, p. 1894–1914, 2024. 

#### PARÂMETROS PARA CADA MODELO ####
#O exercício proposto pelos autores, na verdade, não é um choque sobre o steady-state;
# mas sim partir de um desequilíbrio com diferentes parâmetros iniciais
#por este motivo, o código reproduz os parâmetros para cada um dos 3 cenários que o texto aborda

grid_base_models <- tibble::tibble(
  nome_modelo = c("BUCCHIANICO_2024_a_BASELINE", "BUCCHIANICO_2024_b_schock_i", "BUCCHIANICO_2024_c_schock_phi0"), 
  phi0_v = c(0.03,   0.03,   0.035),
  i_v =    c(0.01,   0.02,   0.01),
  Lw_v =   c(824 ,   924,    824))

# selected_equations <- eqs_bucchianico2024
# period_simulation <- 100

sfcr_scenarios_bucchianico <- function(nome_modelo, phi0_v, i_v, Lw_v, selected_equations = eqs_bucchianico2024, period_simulation = 100){
  
  
  phi0_v = phi0_v
  i_v = i_v
  Lw_v = Lw_v
  # phi0_v = 0.03
  # i_v = 0.01
  # Lw_v = 824
  
  pi_v = 0.39
  cw_v = 0.7
  cr_v = 0.2
  un_v = 0.8242
  v_v = 0.989
  delta_v = 0.084
  gamma_v = 0.15
  phi1_v = 0.1 #Sensitivity of credit-financed consumption to i
  r_v = 0.01 # Amortisation rate


  w_v = 1 - pi_v
  ro_v = 1
  
  u_v = 0.8242  
  d_v = 0.049 #credit financed consumptio to debt rate
  gZw_v = 0.039
  h_v = 0.148
  
  K_v = 100
  
  
  Dw_v = 0
  Dr_v = 0
  D_v = 0
  
  Bw_v = d_v*Lw_v
  Zw_v = Bw_v + (cr_v*i_v - cw_v*(i_v+r_v))*Lw_v
  
  #### PARÂMETROS DEFINIDOS A PARTIR DAS EQUAÇÕES ####
  
  Yfc_v = K_v/v_v
  
  Y_v = Yfc_v*un_v
  FT_v = pi_v*Y_v
  FD_v = ro_v*FT_v
  Fb_v = i_v*Lw_v
  Cr_v = cr_v*(FD_v + Fb_v)
  W_v = w_v*Y_v
  Ydw_v = W_v - (r_v+i_v)*Lw_v
  Ciw_v = cw_v * Ydw_v                                
  Cw_v = Ciw_v + Bw_v                            
  
  FU_v = 0
  I_v = h_v*Y_v # - delta_v*K_v
  E_v = I_v
  Ydr_v = FD_v + Fb_v
  C_v = Cw_v + Cr_v
  Ycalc_v = Cw_v + Cr_v + I_v
  
  SAVw_v = Ydw_v - Cw_v 
  SAVr_v = FD_v + Fb_v - Cr_v 
  SAV_v = SAVw_v + SAVr_v + FU_v
  
  Vr_v = SAVr_v + E_v
  
  ####VALORS INICIAIS####
  parametros_iniciais <- list(
    E = E_v,
    Vr = Vr_v,
    phi0 = phi0_v,
    phi1 = phi1_v,
    i = i_v,
    r = r_v,
    cw = cw_v,
    cr = cr_v,
    pi = pi_v,
    w = w_v,
    gamma = gamma_v,
    ro =ro_v,
    v = v_v,
    delta = delta_v,
    u = u_v,
    un = un_v,
    h = h_v,
    Dw = Dw_v,
    Dr = Dr_v,
    D = D_v,
    Lw = Lw_v,
    K = K_v,
    Zw = Zw_v,
    Yfc = Yfc_v,
    Y = Ycalc_v,
    C = C_v,
    Ciw = Ciw_v,
    Cw = Cw_v,
    Ydw = Ydw_v,
    SAVw = SAVw_v,
    Cr = Cr_v,
    SAVr = SAVr_v,
    Fb = Fb_v,
    FT = FT_v,
    FU = FU_v,
    FD = FD_v,
    SAV = SAV_v,
    I = h_v*Y_v,
    Bw = Bw_v,
    # gZw = gZw_v,
    W = W_v,
    Ydr = Ydr_v
      )
  
  # Transforma em lista de fórmulas
  lista_formula_iniciais <- lapply(names(parametros_iniciais), function(nome) {
    as.formula(paste0(nome, " ~ ", parametros_iniciais[[nome]]))
  })
  
  # Chama sfcr_set via do.call
  init <- do.call(sfcr_set, lista_formula_iniciais)
  
  ####VALORS EXTERNOS####
  
  # Lista nomeada de parâmetros
  parametros_externos <- list(
    phi0 = phi0_v,
    phi1 = phi1_v,
    i = i_v,
    r = r_v,
    cw = cw_v,
    cr = cr_v,
    pi = pi_v,
    w = w_v,
    gamma = gamma_v,
    ro = ro_v,
    v = v_v,
    delta = delta_v,
    un = un_v
  )
  
  # Transforma em lista de fórmulas
  lista_formula_externos <- lapply(names(parametros_externos), function(nome) {
    as.formula(paste0(nome, " ~ ", parametros_externos[[nome]]))
  })
  
  # Chama sfcr_set via do.call
  ext <- do.call(sfcr_set, lista_formula_externos)
  
  print(nome_modelo)
  
  sim_model <- sfcr_baseline(
    equations = selected_equations,
    external  = ext,
    initial   = init,
    periods   = period_simulation
    # hidden = c("Y" = "Yd")
    
    # hidden = NULL
    
  )
  
  
  df_simulation <- sim_model |>
    mutate(
      modelo = nome_modelo
    )
  
  return(df_simulation
   
  )
}


#Consolida resultados 

lista_modelos_baseline <- purrr::pmap(grid_base_models, sfcr_scenarios_bucchianico)
names(lista_modelos_baseline) <- grid_base_models$nome_modelo

# View(lista_modelos_baseline[[1]])
