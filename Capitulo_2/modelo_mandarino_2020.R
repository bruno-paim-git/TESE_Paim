####BIBLIOTECA####
library(sfcr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(purrr)

####  DEFINIÇÃO DE PARÂMETROS INICIAIS ####

##### PARÂMETROS DEFINIDOS MANUALMENTE ####
gEw = 0.050
i = 0.030
r = 0.150
cw = 0.820
cr = 0.700
pi = 0.200
w = 0.800
gamma = 0.009
ro = 0.750 
v = 2.000
delta = 0.020
u = 0.800
un = 0.800
h = 0.175
Dw = 98050
Dr = 46060
D = 144110
Lw = 26460
Lf = 117650
L = 144110
K = 100000
Ew = 5040

##### PARÂMETROS DEFINIDOS A PARTIR DAS EQUAÇÕES ####
Yfc = K/v

Y = Yfc*un
Cw = cw * (w * Y - (r + i) * Lw) + Ew
Yd = w*Y - i*Lw
FT = pi*Y - i*Lf
FD = ro*FT
Fb = i*L
Cr = cr*(FD + Fb)
FU = (1-ro)*FT
I = h*Y

Ycalc = Cw+Cr+I

SAVw = Yd - Cw
SAVr = FD + Fb - Cr 
SAV = SAVw + SAVr + FU

#### SIMULAÇÃO ####

##### SISTEMA DE EQUAÇÕES####
eqs <- sfcr_set(
  Cw ~ cw * (w * Y - (r + i) * Lw[-1]) + Ew,
  Lw ~ Lw[-1] + Ew - r*Lw[-1],
  Yd ~ w*Y - i*Lw[-1],
  SAVw ~ Yd - Cw,
  Dw ~ Dw[-1] + SAVw + Lw - Lw[-1],
  Cr ~ cr*(FD + Fb),
  SAVr ~ FD + Fb - Cr, 
  Fb ~ i*L[-1],
  Dr ~ Dr[-1] + SAVr,
  FT ~ pi*Y - i*Lf[-1],
  FD ~ ro*FT,
  FU ~ (1-ro)*FT,
  I ~ h*Y,
  Yfc ~ K[-1]/v,
  h  ~  h[-1] + h[-1]*gamma*(u[-1] - un),
  K ~ K[-1] + I - delta*K[-1],
  # Lf ~ Lf[-1] + I - FU - r*Lf[-1], #COM AMORTIZAÇÃO DAS FIRMAS
  Lf ~ Lf[-1] + I - FU, #ORIGINAL
  Y ~ Cw + Cr + I,
  Ew ~ Ew[-1]+Ew[-1]*gEw[-1],
  
  D ~ Dw + Dr,
  L ~ Lw + Lf,
  u ~ Y/Yfc,
  SAV ~ SAVw + SAVr + FU,
  
  #FOR BALANCE-SHEET MATRIX
  Vw ~ Dw - Lw,
  Vf ~ K - Lf,
  
  #FOR TRANSACTION-FLOW MATRIX
  W ~ (1-pi)*Y,
  C ~ Cr + Cw,
  
)

#####PARÂMETROS EXÓGENOS####
external <- sfcr_set(
  gEw ~ 0.050,
  i ~ 0.030,
  r ~ 0.150,
  cw ~ 0.820,
  cr ~ 0.700,
  pi ~ 0.200,
  w ~ 0.800,
  gamma ~ 0.009,
  ro ~ 0.750, 
  v ~ 2.000,
  delta ~ 0.020,
  un ~ 0.800,
)
#####VALORS INICIAIS####
initial <- sfcr_set(
  
  gEw ~ 0.050,
  i ~ 0.030,
  r ~ 0.150,
  cw ~ 0.820,
  cr ~ 0.700,
  pi ~ 0.200,
  w ~ 0.800,
  gamma ~ 0.009,
  ro ~ 0.750, 
  v ~ 2.000,
  delta ~ 0.020,
  u ~ 0.800,
  un ~ 0.800,
  h ~ 0.175,
  Dw ~ 98050,
  Dr ~ 46060,
  D ~ 144110,
  Lw ~ 26460,
  Lf ~ 117650,
  L ~ 144110,
  K ~ 100000,
  Ew ~ 5040,
  Yfc ~ Yfc,
  Y ~ Ycalc,
  Cw ~ Cw,
  Yd ~ Yd,
  SAVw ~ SAVw,
  Cr ~ Cr,
  SAVr ~ SAVr,
  Fb ~ Fb,
  FT ~ FT,
  FU ~ FU,
  FD ~ FD,
  SAV ~ SAV,
  I ~ h*Y
)

#####MODELO BASELINE####
mandarino <- sfcr_baseline(
  equations = eqs,
  external  = external,
  initial = initial,
  periods = 500
)
####MATRIZES####
#####BALANCE SHEET####
#Há algum vazamento por não se considerar a amortização dos empréstimos das firmas
bs_mandarino <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r","f", "b", "s"),
  c("Deposits", w = "+Dw", r = "+Dr", b = "-D"),
  c("Loans", w = "-Lw", f = "-Lf", b = "+L"),
  c("Capital Goods", f = "+K", s ="+K"),
  c("Net Worth", w = "Vw", r = "Dr", f = "Vf", s ="+K")
)

sfcr_matrix_display(bs_mandarino, "bs")

sfcr_validate(bs_mandarino, mandarino, "bs", rtol = TRUE, tol = 1e-3)

#####TRANSACTION-FLOW####
tfm_mandarino <- sfcr_matrix(
  columns = c("workers","Rentiers", "Firms_current", "Firms_capital", "Banks"),
  codes = c("w", "r", "fc", "fk", "b"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Wages", w = "+W", fc = "-W"),
  c("Profits", r = "FD + Fb", fc = "-FD", b = "-Fb"),
  c("Interest on loans", w = "-i * Lw[-1]", fc = "-i * Lf[-1]", b = "+i * L[-1]"),
  c("Current saving", w = "SAVw", r = "SAVr", fk = "FU"),
  # c("Current saving", w = "SAVw", r = "SAVr", fc = "FU", fk = "-I"),
  c("Workers' amortization", w = "-r * Lw[-1]", b ="+r * Lw[-1]"),
  # c("Firms' amortization", fc = "-r * Lf[-1]", b ="+r * Lf[-1]"),
  c("Workers' new loans", w = "+Ew", b ="-Ew"),
  c("Ch. Firms' loans", fk = "+d(Lf)", b ="-d(Lf)"),
  c("Ch. Firms' capital", fk = "-I"),
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", b = "+d(D)"),
  c("Ch.Net Worth", w = "d(Vw)", r = "d(Dr)", fk = "d(Lf)"),
  # c("Ch. Net Worth", w = "d(SAVw)", r = "d(SAVr)", fk = "I - FU")
  
)
sfcr_matrix_display(tfm_mandarino)

sfcr_validate(tfm_mandarino, mandarino, which = "tfm", rtol = TRUE, tol = 1e-3)




#####CHOQUE 1: ELEVAÇÃO DE gEw####
shock1 <- sfcr_shock(
  variables = sfcr_set(
    gEw ~ 0.053,
  ),
  start = 10,
  end = 500
)

######Cenário 1####
scenario1 <- sfcr_scenario(
  baseline = mandarino,
  scenario = shock1,
  periods = 500
)

###### GRÁFICOS ####
df <- scenario1  |>
  mutate(cwk= Cw/lag(K),
         crk= Cr/lag(K),
         lwk= Lw/K,
         lfk= Lf/K,
         fuk= FU/lag(K))|> 
  pivot_longer(cols = -period)

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig1 <- df  |> 
  filter(name == "u" | name == "h")

# Separando os dados de 'u' e 'h' para evitar problemas de tamanho
df_1 <- df_fig1 %>% filter(name == "u")
df_2 <- df_fig1 %>% filter(name == "h")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'u', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'h', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of u and h after a positive shock in gEw (Scenario 1)",
    yaxis = list(title = "u"),
    yaxis2 = list(title = "h", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig2 <- df  |> 
  filter(name == "cwk" | name == "crk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig2 %>% filter(name == "cwk")
df_2 <- df_fig2 %>% filter(name == "crk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'cwk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'crk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Cw/k-1 and Cr/k-1 after a positive shock in gEw (Scenario 1)",
    yaxis = list(title = "cwk"),
    yaxis2 = list(title = "crk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig3 <- df  |> 
  filter(name == "lwk" | name == "lfk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig3 %>% filter(name == "lwk")
df_2 <- df_fig3 %>% filter(name == "lfk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'lwk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'lfk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Lw/k and Lf/k after a positive shock in gEw (Scenario 1)",
    yaxis = list(title = "lwk"),
    yaxis2 = list(title = "lfk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig


# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig4 <- df  |> 
  filter(name == "lfk" | name == "fuk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig4 %>% filter(name == "lfk")
df_2 <- df_fig4 %>% filter(name == "fuk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'lfk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'fuk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Lw/k and Lf/k after a positive shock in gEw (Scenario 1)",
    yaxis = list(title = "lfk"),
    yaxis2 = list(title = "fuk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig

#####CHOQUE 2: ELEVAÇÃO DE i####
shock2 <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.040,
  ),
  start = 10,
  end = 500
)

######Cenário 1####
scenario1_shock2 <- sfcr_scenario(
  baseline = mandarino,
  scenario = shock2,
  periods = 500
)

#### GRÁFICOS ####
df <- scenario1_shock2  |>
  mutate(cwk= Cw/lag(K),
         crk= Cr/lag(K),
         lwk= Lw/K,
         lfk= Lf/K,
         fuk= FU/lag(K))|> 
  pivot_longer(cols = -period)

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig1 <- df  |> 
  filter(name == "u" | name == "h")

# Separando os dados de 'u' e 'h' para evitar problemas de tamanho
df_1 <- df_fig1 %>% filter(name == "u")
df_2 <- df_fig1 %>% filter(name == "h")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'u', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'h', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of u and h after a positive shock in i (Scenario 1)",
    yaxis = list(title = "u"),
    yaxis2 = list(title = "h", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig2 <- df  |> 
  filter(name == "cwk" | name == "crk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig2 %>% filter(name == "cwk")
df_2 <- df_fig2 %>% filter(name == "crk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'cwk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'crk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Cw/k-1 and Cr/k-1 after a positive shock in i (Scenario 1)",
    yaxis = list(title = "cwk"),
    yaxis2 = list(title = "crk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig

# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig3 <- df  |> 
  filter(name == "lwk" | name == "lfk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig3 %>% filter(name == "lwk")
df_2 <- df_fig3 %>% filter(name == "lfk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'lwk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'lfk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Lw/k and Lf/k after a positive shock in i (Scenario 1)",
    yaxis = list(title = "lwk"),
    yaxis2 = list(title = "lfk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig


# Supondo que "scenario1" seja o seu dataset
# Pre-processamento com pivot_longer e filter
df_fig4 <- df  |> 
  filter(name == "lfk" | name == "fuk")

# Separando os dados de 'cwk' e 'crk' para evitar problemas de tamanho
df_1 <- df_fig4 %>% filter(name == "lfk")
df_2 <- df_fig4 %>% filter(name == "fuk")

# Criando o gráfico com plotly
fig <- plot_ly()

# Adicionando a variável "u" ao eixo da esquerda
fig <- fig %>% 
  add_lines(x = ~df_1$period, y = ~df_1$value, name = 'lfk', line = list(dash = 'solid'))

# Adicionando a variável "h" ao eixo da direita
fig <- fig %>%
  add_lines(x = ~df_2$period, y = ~df_2$value, name = 'fuk', yaxis = 'y2', line = list(dash = 'dash'))

# Ajustando layout para exibir dois eixos y
fig <- fig %>%
  layout(
    title = "Dynamics of Lw/k and Lf/k after a positive shock in i (Scenario 1)",
    yaxis = list(title = "lfk"),
    yaxis2 = list(title = "fuk", overlaying = "y", side = "right"),
    xaxis = list(title = "Period"),
    legend = list(x = 1, y = 1)
  )

# Exibindo o gráfico interativo
fig



####MULTI_SHOCK####
shock1 <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.04
  ),
  start = 50,
  end = 1000
)


schock_exp <- sfcr_expand(shock1, i, c(seq(from = 0.01, to = 0.06, by = 0.01)))
mandarino_multi <- sfcr_multis(schock_exp, mandarino, periods = 1000, method = "Newton")

mandarino_multi %>%
  bind_rows() %>%
  mutate(ds= ((r+i)*Lw)/(w*Y),
         cw_k = Cw/lag(K),
         log_Y = log(Y),
         c_Y = Cr/Y
         ) |> 
  pivot_longer(cols = -c(period, simulation)) %>%
  filter(name %in% c("c_Y") ) |>
  filter(period >3 & period <500  ) |> 
    plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(simulation, labels = as.character(c(seq(from = 0.01, to = 0.06, by = 0.01)))),  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "i"),  # Set the y-axis label
    legend = list(title = list(text = "Choques em i"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )

