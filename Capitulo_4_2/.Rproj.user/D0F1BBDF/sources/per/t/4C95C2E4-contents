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

#########################################################################
#########################################################################
#########################################################################
#Este capítulo é formado por 8 scripts:
# 01_tese_equations
# equações do modelo supermultiplicados
#
# 02_tese_auxiliary_functions
# equações auxiliares:
#     - process_model_df: cria variáveis auxiliares nas simulações
#     - gerar_subplot: cria subplots e plotly
#     - gerar_subplot_ggplot: cria subplots em ggplot
#     - salvar_subplot: salva subplots para tese
#     - resolver parâmetros para Lz/Y de estado estacionário
#
# 03_tese_simulation
# simulação com 6 modelos:
#     - 6 combinações de beta e phi0
#     - definição de parâmetros iniciais
#     - definição de parâmetros exógenos
#     - cria lista com simulações
#
# 04_tese_shocks
# aplica choques sobreshocks:
#     - choque na taxa de juros (i)
#     - choque na taxa de crescimento do componente autônomo (phi0)
#     - choque no limite de crédito aos trabalhadores (beta)
#
# 05_tese_sensitivity_beta_i
# aplica testes de sensibilidade sobre valor da taxa de juros (i) e com diferentes limite de crédito aos trabalhadores (beta)
#
# 06_tese_sensitivity_alpha_beta
# aplica testes de sensibilidade sobre valor do crescimento autonomo (phi_0) com diferentes limite de crédito aos trabalhadores (beta)
#
# 07_function_sfcr_sankey
# corrige um problema na função sfcr_sankey do pacote sfcr
#
# 08_tese_matrix
# cria as matrizes:
#     - balance-sheet
#     - transaction-flow
#     - Directed Acyclic Graph
#     - Sankey Diagram
#########################################################################
#########################################################################
#########################################################################

####MODELO TESE####

{eqs_K <- sfcr_set(
  # 1. RENDA DISPONÍVEL REGULAR (SNA/Contábil - usada na TFM e Poupança)
  Ydw ~ W - J,                                                           #OK
  
  J ~ icc_b * Lb[-1] + icc_z * Lz[-1],                                   #OK
  
  # i_z ~ 0.014 + lambda_z*i,                                            #DESCONSIDERADO
  # i_b ~ 2.248 + lambda_b*i,                                            #DESCONSIDERADO
  i_z ~ lambda_z*i,                                                      #OK
  i_b ~ lambda_b*i,                                                      #OK
  
  icc_z ~ icc_z[-1] + r_z*(i_z - icc_z[-1]),                             #OK
  icc_b ~ icc_b[-1] + r_b*(i_b - icc_b[-1]),                             #OK
  
  # 2. AMORTIZAÇÃO TOTAL OBRIGATÓRIA DA DÍVIDA
  A ~ r_b * Lb[-1] + r_z * Lz[-1],                                       #OK
  
  #RENDA DISPONÍVEL DOS TRABALHADORES
  Ydw_linha ~ Ydw - A,                                                   #OK
  
  #RENDA DOS SALÁRIOS
  W ~ w*Y,                                                               #OK
  
  #CONSUMO INDUZIDO A PARTIR DA RENDA
  Ciw ~ cw * Ydw_linha,                                                  #OK
  
  #CONSUMO INDUZIDO FINANCIADO POR CRÉDITO
  Bw ~ pmax(0, r_b*Lb[-1] + beta*(W - W[-1])),                           #OK
  
  #CONSUMO AUTÔNOMO
  Zw ~ Zw[-1]*(1+gZ),                                                    #OK
  gZ ~ phi0 - phi1*i,                                                    #OK
  
  #CONSUMO TOTAL DOS TRABALHADORES
  Cw ~ Ciw + Bw + Zw,                                                    #OK
  
  #SALDO  DA DÍVIDA CRÉDITO INDUZIDO
  Lb ~ Bw + (1 - r_b)*Lb[-1],                                            #OK
  
  #SALDO  DA DÍVIDA CRÉDITO AUTÔNOMO
  Lz ~ Zw + (1 - r_z)*Lz[-1],                                            #OK
  
  #SALDO  DA DÍVIDA DOS TRABALHADORES
  L ~ Lb + Lz,                                                           #OK
  
  #POUPANÇA DOS TRABALHADORES
  SAVw ~ Ydw - Cw ,                                                      #OK
  
  #DEPÓSITOS DOS TRABALHADORES
  Dw ~ Dw[-1] + SAVw + Bw + Zw - A,                                      #OK

  ####EMPRESAS E BANCOS####
  #INVESTIMENTO
  I ~ h*Y,                                                               #OK
  
  #PROPENSÃO MARGINAL A INVESTIR
  h ~ h[-1] + h[-1]*gamma*(u - un),                                  #OK

  #ESTOQUE DE CAPITAL
  K ~ K[-1] + I - delta*K[-1],                                           #OK
  
  # LUCRO DAS EMPRESAS 
  FT ~ pi*Y,                                                             #OK
  
  #LUCROS DISTRIBUÍDOS
  FD ~ FT,                                                               #OK
  
  #AÇÕES                                                            
  E ~ E[-1] + I,                                                         #OK
  
  #LUCRO DOS BANCOS
  Fb ~ J,                                                                #OK
  
  
  #### RENTISTAS ####
  #RENDA DISPONÍVEL PARA OS RENTISTAS
  Ydr ~ Fb + FD,                                                         #OK
  
  #CONSUMO DOS RENTISTAS
  Cr ~ cr*Ydr,                                                           #OK
  
  #POUPANÇA DOS RENTISTAS
  SAVr ~ Ydr - Cr,                                                       #OK
  
  #DEPÓSITOS DOS RENTISCAS
  Dr ~ Dr[-1] + SAVr - (E - E[-1]),                                      #OK
  
  ####ATIVIDADE####
  #UTILIZAÇÃO DA CAPACIDADE
  u ~ Y/Yfc,                                                             #OK
  
  #PRODUTO DE PLENA CAPACIDADE
  Yfc ~ K[-1]/v,                                                         #OK
  
  ######FECHAMENTO####
  C ~ Cw + Cr,                                                           #OK
  Y ~ C + I,                                                             #OK
  D ~ Dw + Dr,                                                           #OK
  SAV ~ SAVw + SAVr,                                                     #OK
  
  # PARA MATRIZ BALANCE-SHEET
  Vw ~ Dw - L,                                                           #OK
  Vr ~ Dr + E,                                                           #OK
  Vf ~ K - E,                                                            #OK
  
  
  #hidden
  delta_L ~ L - L[-1],                                                   #HIDDEN EQUATION
  delta_D ~ D - D[-1]                                                    #HIDDEN EQUATION
)
}



