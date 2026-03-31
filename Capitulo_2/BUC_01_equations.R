
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
#Foram adaptadas algumas equações com base em DOMENICO et al. (2024)
#uma das adaptações foi fazer o consumo e o investimento correntes se formar a partir da renda defasada

#####SISTEMA DE EQUAÇÕES - BUCCHIANICO 2024####
eqs_bucchianico2024 <- sfcr_set(
  
  #####TRABALHADORES####
  Ciw ~ cw * Ydw[-1],  #ATENÇÃO
  # Ciw ~ cw * Ydw,  #ATENÇÃO
  W ~ w*Y,
  Ydw ~ W  - (r+i)*Lw[-1],         
  Bw ~ Bw[-1] + (phi0 - phi1*i)*Bw[-1],
  Lw ~ Lw[-1] + Bw - r*Lw[-1],
  Cw ~ Bw + Ciw,
  Zw ~ Bw + (cr*i - cw*(i+r))*Lw[-1], #equação está errada no original
  
  SAVw ~ Ydw - Cw , 
  Dw ~ Dw[-1] + SAVw + Lw - Lw[-1] ,
  
  
  ######CAPITALISTAS####
  Cr ~ cr*Ydr[-1],      #ATENÇÃO
  # Cr ~ cr*Ydr,      #ATENÇÃO
  Ydr ~ FD + Fb,
  Fb ~ i*Lw[-1],
  FT ~ pi*Y,
  FD ~ ro*FT,
  
  
  SAVr ~ Ydr - Cr - (E - E[-1]),
  Dr ~ Dr[-1] + SAVr,
  
  
  ######EMPRESAS####
  I ~ h*Y[-1],   #ATENÇÃO
  # I ~ h*Y,   #ATENÇÃO
  h ~ h[-1] + h[-1]*gamma*(u[-1] - un),   #ATENÇÃO
  # h ~ h[-1] + h[-1]*gamma*(u - un),   #ATENÇÃO
  
  K ~ K[-1] + I - delta*K[-1],
  FU ~ (1-ro)*FT,
  E ~ I + E[-1],
  
  ######ATIVIDADE####

  u ~ Y/Yfc,
  Yfc ~ K/v,
  
  ######FECHAMENTO####
  C ~ Cw + Cr,
  Y ~ C + I,
  D ~ Dw + Dr,
  SAV ~ SAVw + SAVr,
  
  # gZw ~ Zw/Zw[-1] -1,
  
  # #FOR BALANCE-SHEET MATRIX
  Vw ~ Dw - Lw,
  Vr ~ Dr + E, #alterado
  
  #FOR TRANSACTION-FLOW MATRIX
  d ~ Bw/Lw
  
)

