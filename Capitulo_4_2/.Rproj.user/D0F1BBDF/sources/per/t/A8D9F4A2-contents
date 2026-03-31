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

#########################################################################
#########################################################################
#########################################################################
#Este capítulo é formado por 5 scripts:
# 01_tese_equations
# equações do modelo supermultiplicados
#
# 02_tese_parameters_and_simulation_cenarios
# simulação com 6 modelos:
#     - 6 combinações de beta e phi0
#     - definição de parâmetros iniciais
#     - definição de parâmetros exógenos
#     - cria lista com simulações
#
# 03_tese_auxiliary_functions
# equações auxiliares:
#     - process_model_df: cria variáveis auxiliares nas simulações
#     - gerar_subplot: cria subplots e plotly
#     - gerar_subplot_ggplot: cria subplots em ggplot
#     - salvar_subplot: salva subplots para tese
#
# 04_tese_shocks
# aplica choques sobreshocks:
#     - choque na taxa de juros (i)
#     - choque na taxa de crescimento do componente autônomo (phi0)
#     - choque no limite de crédito aos trabalhadores (beta)
#
# 05_tese_sensitivity:
# aplica testes de sensibilidade sobre valor da taxa de juros (i) e limite de crédito aos trabalhadores (beta)
#
# 06_function_sfcr_sankey
# corrige um problema na função sfcr_sankey do pacote sfcr
#
# 07_tese_matrix
# cria as matrizes:
#     - balance-sheet
#     - transaction-flow
#     - Directed Acyclic Graph
#     - Sankey Diagram
#########################################################################
#########################################################################
#########################################################################

####MODELO TESE####
# 
#seguindo Domenico et al (2024) os salários são função da renda passada
#semelhante à BUCCHIANICO

{eqs_K <- sfcr_set(
  
  #CONSUMO DOS TRABALHADORES
  Ciw ~ cw * Ydw, #OK
  Bw ~ if(!is.na (delta_W) && delta_W < 0) {0} else {r_b*Lbw[-1] + beta*delta_W}, #OK
  Zw ~ Zw[-1]*(1+gZ), #OK
  gZ ~ phi0 - phi1*lambda_z*i,  #OK
  Cw ~ Ciw + Bw + Zw, #OK
  W ~ w*Y[-1], #OK
  delta_W ~ W - W[-1],
  Ydw ~ W  - (r_b+lambda_b*i)*Lbw[-1] - (r_z+lambda_z*i)*Lzw[-1], #OK
  
  Lbw ~ Lbw[-1] + Bw - r_b*Lbw[-1], #OK
  Lzw ~ Lzw[-1] + Zw - r_z*Lzw[-1], #OK
  Lw ~ Lbw + Lzw, #OK
  
  
  SAVw ~ Ydw - Cw ,  #OK
  Dw ~ Dw[-1] + SAVw,#  #OK
  
  
  ######EMPRESAS####
  I ~ h*Y,   #OK
  h ~ h[-1] + h[-1]*gamma*(u[-1] - un), #OK
  K ~ K[-1] + I - delta*K[-1], #OK
  FU ~ (1-ro)*FT, #OK considerado ro = 1 no modelo
  E ~ E[-1] + I - FU, #OK
  
  
  
  ######RENTISTAS####
  Cr ~ cr*Ydr, #OK
  Ydr ~ FD + Fb, #OK
  Fb ~ i*(lambda_b*Lbw[-1] + lambda_z*Lzw[-1] ), #OK
  FT ~ pi*Y, #OK
  FD ~ ro*FT, #OK
  
  SAVr ~ Ydr - Cr - (E - E[-1]), #OK
  Dr ~ Dr[-1] + SAVr, #OK
  
  
  ######ATIVIDADE####
  u ~ Y/Yfc, #OK
  Yfc ~ K[-1]/v, #OK
  
  ######FECHAMENTO####
  C ~ Cw + Cr, #OK
  Y ~ C + I, #OK
  D ~ Dw + Dr,
  SAV ~ SAVw + SAVr + FU, #OK
  
  # #FOR BALANCE-SHEET MATRIX
  Vw ~ Dw - Lw, #OK
  Vr ~ Dr + E, #OK
)}

