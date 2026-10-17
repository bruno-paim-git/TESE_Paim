
#Exercício para reproduzir com o pacote sfcr o artigo:
#DI BUCCHIANICO, Stefano; GALLO, Ettore; LOFARO, Antonino.
#Debt-credit Flows and Stocks in a Supermultiplier Model with Two Autonomous Demand Components: Consequences for Growth.
#Review of Political Economy, [s. l.], v. 36, n. 5, p. 1894–1914, 2024. 


####BIBLIOTECA####
library(sfcr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(purrr)
library(tidyverse)
library(ggraph)

#### EQUAÇÕES ####
#A adaptação do artigo original passa pela tentativa de tornar um modelo em tempo contínuo em outro de tempo discreto.
#O original estabelece o período de transição ao estado estacionário a partir das equações diferenciais.
#Esta adaptação transforma as equações diferenciais em discretas, estabelecendo um fluxo entre os valores absolutos de cada período.
#Neste sentido, algumas decisões quanto a quais variáveis são defasadas exigiram certo nível de dedução.
#Não por outro motivo, os resultados obtidos, apesar de se aproximarem aos apresentados pelo artigo original, não são idênticos.


#####SISTEMA DE EQUAÇÕES - BUCCHIANICO 2024####
eqs_bucchianico2024 <- sfcr_set(
  
  #####TRABALHADORES####
  #CONSUMO INDUZIDO A PARTIR DA RENDA
  Ciw ~ cw * Ydw,
  
  #RENDA DOS SALÁRIOS
  W ~ w*Y,
  
  #RENDA DISPONÍVEL DOS TRABALHADORES
  Ydw ~ W  - (r+i)*Lw[-1],
  
  #CONSUMO AUTÔNOMO
  Bw ~ (1+gB)*Bw[-1],
  gB ~ phi0 - phi1*i,
  
  #SALDO  DA DÍVIDA CRÉDITO AUTÔNOMO
  Lw ~ Bw + (1- r)*Lw[-1],
  
  #CONSUMO TOTAL DOS TRABALHADORES
  Cw ~ Bw + Ciw,
  
  #POUPANÇA DOS TRABALHADORES
  SAVw ~ Ydw - Cw, 
  
  #DEPÓSITOS DOS TRABALHADORES
  Dw ~ Dw[-1] + SAVw + Bw,
  
  ######EMPRESAS E BANCOS####
  #INVESTIMENTO
  I ~ h*Y,   
  
  #PROPENSÃO MARGINAL A INVESTIR
  h ~ h[-1] + h[-1]*gamma*(u - un),
  
  #ESTOQUE DE CAPITAL
  K ~ K[-1] + I - delta*K[-1],
  
  # LUCRO DAS EMPRESAS 
  FT ~ pi*Y,
  
  #LUCRO DOS BANCOS
  Fb ~ i*Lw[-1],
  
  #LUCRO RETIDO DAS EMPRESAS
  FU ~ (1-ro)*FT,   #COMO ro = 1, FU = 0
  
  #LUCRO DISTRIBUÍDOS DAS EMPRESAS
  FD ~ ro*FT,
  
  #AÇÕES                                                            
  E ~ I + E[-1] - FU,
  
  ######CAPITALISTAS####
  #RENDA DISPONÍVEL PARA OS RENTISTAS
  Ydr ~ Fb + FD,
  
  #CONSUMO DOS RENTISTAS
  Cr ~ cr*Ydr,
  
  #POUPANÇA DOS RENTISTAS
  SAVr ~ Ydr - Cr,
  
  #DEPÓSITOS DOS RENTISCAS
  Dr ~ Dr[-1] + SAVr - (E - E[-1]),  
  
  ######ATIVIDADE####
  #UTILIZAÇÃO DA CAPACIDADE
  u ~ Y/Yfc,
  
  #PRODUTO DE PLENA CAPACIDADE
  Yfc ~ K[-1]/v,
  
  ######FECHAMENTO####
  C ~ Cw + Cr,
  Y ~ C + I,
  D ~ Dw + Dr,
  SAV ~ SAVw + SAVr,           
  
  # #FOR BALANCE-SHEET MATRIX
  Vw ~ Dw - Lw,
  Vr ~ Dr + E,
  Vf ~ K - E,
  
  #FOR TRANSACTION-FLOW MATRIX
  d ~ Bw/Lw[-1]
)

