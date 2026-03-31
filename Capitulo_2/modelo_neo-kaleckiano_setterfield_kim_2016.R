#Exercício para reproduzir com o pacote sfcr o artigo:
#SETTERFIELD, M.; KIM, Y. K. Debt servicing, aggregate consumption, and growth.
#Structural Change and Economic Dynamics, [s. l.], v. 36, p. 22–33, 2016.
#https://doi.org/10.1016/j.strueco.2015.10.002

####BIBLIOTECA####
library(sfcr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(ggraph)
library(webshot) #para exportar gráficos em plotly


# Instale primeiro: webshot::install_phantomjs()

####  DEFINIÇÃO DE PARÂMETROS INICIAIS ####

##### PARÂMETROS DEFINIDOS MANUALMENTE  ####

######Parâmetro Fixos####
cw = 0.940
cr = 0.200
beta = 0.100
alfa = 0.25
kappa_0 = 0.045
kappa_r = 0.5
i = 0.0481

u = 0.8
N = 1

# Neoliberal regime parameters
wr = 0.23835 # wr = phi*alfa*wp
wp = 0.42 # wp = (1-pi)/(1+phi*alfa)
pi = 0.34165
eta = 21.72 # lambda*delta
phi = 2.27


wr+wp+pi

###### Valores Iniciais definidos a partir das equações ####
K = 100
Y = K*u
r = pi*u
PI = r*K
Wp = wp*Y/N
Wr = phi*Wp
N = ((Y- PI)/(Wp+Wr*alfa))
W = Wp*N + Wr*alfa*N
Ydw = Wp*N
Dr = Dw = L = 0# Dívida inicial = 0
Ydp = Wr*alfa*N + PI + i*Dr
Cw = cw*Ydw
Cr = cr*Ydp
Ct = eta*Cr
B = beta*(Ct-Cw)
gk = kappa_0 + kappa_r*r
I = gk*K
C = Cw + Cr + B
Y = C + I
SAVw = Ydw - Cw - i*Dr
SAVr = Ydp - Cr

####NEOLIBERAL####
#####PARÂMETROS EXÓGENOS####
external <- sfcr_set(
  cw ~ 0.940,
  cr ~ 0.200,
  beta ~ 0.100,
  phi ~ 2.27,
  alfa ~ 0.25,
  kappa_0 ~ 0.045,
  kappa_r ~ 0.5,
  i ~ 0.0481,
  N ~ 1,
  
  ###### Parâmetros dos Regimes####
  # Neoliberal regime
  eta ~ 21.72, # lambda*delta
  pi ~ 0.34165,
  wr ~ 0.23835, # wr = phi*alfa*wp
  wp ~ 0.42, # wp = (1-pi)/(1+phi*alfa)
  
)

#####VALORS INICIAIS####
initial <- sfcr_set(
  u ~ u,
  Wr ~ Wr,
  Wp ~ Wp,
  gk ~ gk,
  r ~ r,
  PI ~ PI,
  Y ~ Y,
  C ~ C,
  L ~ L,
  Dr ~ Dr,
  Dw ~ Dw,
  Ct ~ Ct,
  Cw ~ Cw,
  Cr ~ Cr,
  K ~ K
)
#####EQUAÇÕES####
eqs <- sfcr_set(
  #salarios reais
  Wp ~ wp*Y/N,
  Wr ~ phi*Wp,
  W ~ Wp + Wr,
  #rendas disponíveis
  Ydw ~ Wp*N,
  Ydp ~ Wr*alfa*N + PI + i*Dr[-1],

  #consumo
  Cw ~ cw*Ydw,
  Cr ~ cr*Ydp,
  Ct ~ eta*Cr,
  B ~ beta*(Ct-Cw),
  
  C ~ Cw + Cr + B,
  
  #investimento e lucro
  gk ~ kappa_0 + kappa_r*r[-1],
  K ~ K[-1]*(1+gk),
  I ~ gk*K[-1],
  PI~ r*K[-1],
  E ~ E[-1] - I,
  
  #atividade
  u ~ Y/K[-1],
  r ~ pi*u,

  Y ~ C + I,
  
  #poupanças
  SAVw ~ (1-cw)*Ydw - i*Dr[-1],
  # SAVw ~ Ydw - Cw - i*D[-1] + i*Dw[-1],

  SAVr ~ (1-cr)*Ydp -(E-E[-1]),
  # SAVr ~ (1-cr)*Ydp,

  SAV ~ SAVw + SAVr,
  
  #depósitos
  L ~ L[-1] + B ,
  Dw ~ Dw[-1] + SAVw,
  Dr ~ L - Dw,

  ##extras###
  c ~ (1-cw)*wp*u-i*dr,
  sust ~ dr - dr[-1],
  dr ~ Dr/K,
  dr2 ~ L/K,
  dry ~ Dr/Y
)

####MODELO####
SK2016 <- sfcr_baseline(
  equations = eqs,
  external  = external,
  initial = initial,
  periods = 100,
  tol = 1e-15,
  rhtol = TRUE,
  .hidden_tol = 1e-6
)


#####PLOTLY |Gráfico com as principais variáveis####

dados <- SK2016 |> filter(period < 100)

# Último período para cada série
ultimo_ponto <- dados |> filter(period == max(period))

plot_setterfield_neoliberal_plotly <- plot_ly(dados, x = ~period, y = ~dr, type = 'scatter', mode = 'lines',
        name = 'Razão Dívida/Estoque de Capital (dr)',
        line = list(color = 'steelblue', width = 2)) %>%
  
  add_trace(y = ~u, name = 'Grau de utilização da Capacidade Instalada (u)',
            mode = 'lines', line = list(color = 'forestgreen', dash = 'solid', width = 2)) %>%
  
  add_trace(y = ~c, name = 'Coeficiente de viabilidade (c)',
            mode = 'lines', line = list(color = 'firebrick', dash = 'solid', width = 2)) %>%
  
  add_trace(y = ~sust, name = 'Condição de Sustentabilidade (sust)',
            mode = 'lines', line = list(color = 'black', dash = 'solid', width = 2)) %>%
  
  # Anotações fixas nos últimos pontos
  add_annotations(x = ultimo_ponto$period, y = ultimo_ponto$dr,
                  text = paste0("dr: ", round(ultimo_ponto$dr, 3)),
                  showarrow = FALSE, xanchor = 'left', yanchor = 'middle',
                  font = list(size = 14, color = 'steelblue')) %>%
  
  add_annotations(x = ultimo_ponto$period, y = ultimo_ponto$u,
                  text = paste0("u: ", round(ultimo_ponto$u, 3)),
                  showarrow = FALSE, xanchor = 'left', yanchor = 'middle',
                  font = list(size = 14, color = 'forestgreen')) %>%
  
  add_annotations(x = ultimo_ponto$period, y = ultimo_ponto$c,
                  text = paste0("c: ", round(ultimo_ponto$c, 3)),
                  showarrow = FALSE, xanchor = 'left', yanchor = 'middle',
                  font = list(size = 14, color = 'firebrick')) %>%
  
  add_annotations(x = ultimo_ponto$period, y = ultimo_ponto$sust,
                  text = paste0("sust: ", round(ultimo_ponto$sust, 3)),
                  showarrow = FALSE, xanchor = 'left', yanchor = 'middle',
                  font = list(size = 14, color = 'black')) %>%
  
  layout(
    xaxis = list(title = 'Períodos', titlefont = list(size = 20), tickfont = list(size = 16)),
    yaxis = list(title = 'Variáveis', titlefont = list(size = 20), tickfont = list(size = 16)),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 18)),
    margin = list(b = 100),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )

plot_setterfield_neoliberal_plotly

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(plot_setterfield_neoliberal_plotly, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\plot_setterfield_neoliberal.png"), vwidth = 1600, vheight = 800)


#####GGPLOT |Gráfico com as principais variáveis#####
# Último período
ultimo_ponto <- dados |> 
  filter(period == max(period))

# Transformar para formato longo (facilita o ggplot)
dados_long <- dados |> 
  pivot_longer(cols = c(dr, u, c, sust),
               names_to = "variavel",
               values_to = "valor")

# Labels para legenda
labels_variaveis <- c(
  dr   = "Razão Dívida/Estoque de Capital (dr)",
  u    = "Grau de utilização da Capacidade Instalada (u)",
  c    = "Coeficiente de viabilidade (c)",
  sust = "Condição de Sustentabilidade (sust)"
)

# Cores equivalentes
cores <- c(
  dr   = "steelblue",
  u    = "forestgreen",
  c    = "firebrick",
  sust = "black"
)

# Gráfico
plot_setterfield_neoliberal_ggplot <- ggplot(dados_long, 
                                      aes(x = period, y = valor, color = variavel)) +
  geom_line(size = 1) +
  
  scale_color_manual(values = cores, labels = labels_variaveis) +
  
  # Anotações no último ponto
  geom_text(data = ultimo_ponto,
            aes(x = period, y = dr, 
                label = paste0("dr: ", round(dr, 3))),
            color = "steelblue", hjust = -0.5, size = 5) +
  
  geom_text(data = ultimo_ponto,
            aes(x = period, y = u, 
                label = paste0("u: ", round(u, 3))),
            color = "forestgreen", hjust = -0.5, size = 5) +
  
  geom_text(data = ultimo_ponto,
            aes(x = period, y = c, 
                label = paste0("c: ", round(c, 3))),
            color = "firebrick", hjust = -0.5, size = 5) +
  
  geom_text(data = ultimo_ponto,
            aes(x = period, y = sust, 
                label = paste0("sust: ", round(sust, 3))),
            color = "black", hjust = -0.5, size = 5) +
  
  labs(
    x = "Períodos",
    y = "Variáveis",
    color = NULL
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    plot.margin = margin(b = 40)
  ) +
  
  # Evita corte das labels à direita
  expand_limits(x = max(dados$period) * 1.1)

plot_setterfield_neoliberal_ggplot

ggsave(filename = paste0(".\\Output\\plot_setterfield_neoliberal_ggplot.svg"), plot = plot_setterfield_neoliberal_ggplot, width = 16, height = 8, dpi = 800)


####PLOTLY CHOQUES EM ETA - 1####
#reproduz choques em eta idênticos ao do artigo
#####Define o Choque####
shock_eta <- sfcr_shock(
  variables = sfcr_set(
    eta ~ 0.29
  ),
  start = 15,
  end = 500
)

#####Expande o Choque####

eta_shock <- sfcr_expand(shock_eta, eta, c(0.29, 2.92, 10, 21.72))

#####Simula os Choques####

mlt_sk2016_table6 <- sfcr_multis(eta_shock, SK2016, 500)

#####Gráfico em plotly####

mlt_sk2016_table6 %>% 
  bind_rows() %>%
  pivot_longer(cols = -c(period, simulation)) %>%
  filter(name %in% c("dr")) %>%
  plot_ly(
    x = ~period,
    y = ~value,
    color = ~factor(simulation, labels = as.character(c(0.29, 2.92, 10, 21.72))) ,  # Custom labels
    colors = "Set1"
  ) %>%
  add_lines() %>%
  layout(
    xaxis = list(title = "Períodos"),  # Set the x-axis label
    yaxis = list(title = "dr"),  # Set the y-axis label
    legend = list(title = list(text = "Choques em eta"), orientation = "h", x = 0.5, xanchor = "center", y = -0.2)  # legend position at the bottom
  )

####CHOQUES EM ETA - 2####
# exercício apresentado na tese #
#####Expande o Choque####
multi_shock <- sfcr_expand(shock_eta, eta, sort(c(seq(4, 20, 4), 0.29, 2.92, 21.72)) )

#####Simula os Choques####

multi_sk2016 <- sfcr_multis(multi_shock, SK2016, 500)

#####Gráfico em plotly####

# 1. Dados
dados_plot <- multi_sk2016 %>%
  bind_rows() %>%
  pivot_longer(cols = -c(period, simulation)) %>%
  filter(name == "dr")

# 2. Obter o último ponto de cada simulação
ultimo_ponto <- dados_plot %>%
  group_by(simulation) %>%
  filter(period == max(period)) %>%
  ungroup()

# 3. Criar labels personalizados (com correspondência direta)
unique_sim <- sort(unique(dados_plot$simulation))
custom_labels <- as.character(sort(c(seq(4, 20, 4), 0.29, 2.92, 21.72)))

# Garantir que número de labels = número de simulações
if (length(unique_sim) != length(custom_labels)) {
  stop("Número de labels não corresponde ao número de simulações.")
}

label_map <- setNames(custom_labels, unique_sim)

# 4. Adicionar a coluna com o label correspondente
dados_plot$sim_label <- label_map[as.character(dados_plot$simulation)]
ultimo_ponto$sim_label <- label_map[as.character(ultimo_ponto$simulation)]

# 5. Criar gráfico
plot_setterfield_eta_shock <- plot_ly(
  data = dados_plot,
  x = ~period,
  y = ~value,
  color = ~sim_label,
  colors = "Set1",
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    xaxis = list(title = 'Períodos', titlefont = list(size = 20), tickfont = list(size = 16)),
    yaxis = list(title = 'Razão Dívida/Estoque de Capital (dr)', titlefont = list(size = 20), tickfont = list(size = 16)),
    showlegend = FALSE,
    margin = list(b = 100),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white'
  )

  

# 6. Adicionar anotações fixas no último ponto
for (i in 1:nrow(ultimo_ponto)) {
  plot_setterfield_eta_shock <- plot_setterfield_eta_shock %>%
    add_annotations(
      x = ultimo_ponto$period[i],
      y = ultimo_ponto$value[i],
      text = paste0("eta ", ultimo_ponto$sim_label[i], "<br>dr = ",round(ultimo_ponto$value[i], 3)),
      showarrow = FALSE,
      xanchor = "left",
      yanchor = "middle",
      font = list(size = 14)
    )
}

plot_setterfield_eta_shock

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(plot_setterfield_eta_shock, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\plot_setterfield_eta_shock.png"), vwidth = 1600, vheight = 800)

####GGPLOT | CHOQUES EM ETA - 2####

# 1. Dados
dados_plot <- multi_sk2016 %>%
  bind_rows() %>%
  pivot_longer(cols = -c(period, simulation)) %>%
  filter(name == "dr")

# 2. Último ponto por simulação
ultimo_ponto <- dados_plot %>%
  group_by(simulation) %>%
  filter(period == max(period)) %>%
  ungroup()

# 3. Labels personalizados
unique_sim <- sort(unique(dados_plot$simulation))
custom_labels <- as.character(sort(c(seq(4, 20, 4), 0.29, 2.92, 21.72)))

if (length(unique_sim) != length(custom_labels)) {
  stop("Número de labels não corresponde ao número de simulações.")
}

label_map <- setNames(custom_labels, unique_sim)

# 4. Aplicar labels
dados_plot$sim_label <- label_map[as.character(dados_plot$simulation)]
ultimo_ponto$sim_label <- label_map[as.character(ultimo_ponto$simulation)]

# 5. Gráfico
plot_setterfield_eta_shock <- ggplot(dados_plot,
                                     aes(x = period,
                                         y = value,
                                         group = simulation,
                                         color = sim_label)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0)+
  # Paleta equivalente ao Set1 do plotly
  scale_color_brewer(palette = "Set1") +
  
  # Anotações no último ponto
  geom_text(data = ultimo_ponto,
            aes(label = paste0("eta ", sim_label,
                               "\ndr = ", round(value, 3))),
            hjust = -0.1,
            size = 4) +
  
  labs(
    x = "Períodos",
    y = "Razão Dívida/Estoque de Capital (dr)"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",   # remove legenda
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    plot.margin = margin(b = 40)
  ) +
  
  # Espaço à direita para os labels
  expand_limits(x = max(dados_plot$period) * 1.1)

plot_setterfield_eta_shock
ggsave(filename = paste0(".\\Output\\plot_setterfield_eta_shock_ggplot.svg"), plot = plot_setterfield_eta_shock, width = 16, height = 8, dpi = 800)

####MATRIZES####
#####BALANCE SHEET####
bs_SK2016 <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r","f", "b", "s"),
  c("Capital", f = "+K", s = "+K"), 
  c("Deposits", w = "+Dw", r = "+Dr", b = "-Dw-Dr"),
  c("Loans", w = "-L", b = "+L"),
  c("Equity", r = "+E", f ="-E"),
  c("Net Worth", w = "+Dw-L", r = "+Dr+E", f = "+K-E", b = "+L -(Dw+Dr)", s ="+K"),
)

sfcr_matrix_display(bs_SK2016, "bs")

sfcr_validate(bs_SK2016, SK2016, "bs", rtol = TRUE,  tol = 1e-3)

#####TRANSACTION-FLOW####
tfm_SK2016 <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms current", "Firms capital", "Banks current", "Banks capital"),
  codes = c("w", "r", "fc", "fk", "bc", "bk"),
  c("Consumption by wage", w = "-Cw", r = "-Cr", fc = "+Cw+Cr"),
  c("Consumption by debt", w = "-(L - L[-1])", fc = "+(L - L[-1])"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Wage", w = "Wp", r = "Wr", fc = "-W"),
    c("Profits", r = "+PI", fc = "-PI"),
  c("Deposit Interest", w = "+i*Dw", r = "+i*Dr", bc = "-i*(Dw+Dr)"),
  c("Loan Interest", w = "-i*L", bc = "+i*L"),
  c("Ch. Deposits", w = "-(Dw - Dw[-1])", r = "-(Dr - Dr[-1])", bk = "+(Dw - Dw[-1])+(Dr - Dr[-1])"),
  c("Ch. Loans", w = "+(L-L[-1])", bk = "-(L-L[-1])"),
  c("Issues of equities", r = "-(E-E[-1])", fk = "+(E-E[-1])"),
)
sfcr_matrix_display(tfm_SK2016)

sfcr_validate(tfm_SK2016, SK2016, which = "tfm", rtol = TRUE, tol = 1e-3)

####Structure of the model (DAG)####

sfcr_dag_blocks_plot(eqs)
sfcr_dag_cycles_plot(eqs, size = 10)


#####SANKEY'S DIAGRAM####
#é necessário rodar o arquivo "funtion_sfcr_sankey.R" que corrige um detalhe da função original
source("funtion_sfcr_sankey.R")
sfcr_sankey(tfm_SK2016, SK2016, when = "start")

sfcr_sankey(tfm_SK2016, SK2016, when = "end")

### RESULTADO EM CSV###
# 
# write.csv2(SK2016,
#            file =".\\sk_2016.csv" )
