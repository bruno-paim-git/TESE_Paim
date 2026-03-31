# #INSPIRAÇÃO: https://macrosimulation.org/a_sraffian_supermultiplier_model
#FONTE: SERRANO, Franklin; FREITAS, Fabio; BHERING, Gustavo.
#The Trouble with Harrod: The fundamental instability of the warranted rate in the light of the Sraffian Supermultiplier.
#Metroeconomica, [s. l.], v. 70, n. 2, p. 263–287, 2019. 

####BILBIOTECA####


library(dplyr)
library(ggplot2)
library(reshape2)  # ou tidyr, mas melt é rápido para matriz

####SIMULATION CODE####
# #Clear the environment
rm(list=ls(all=TRUE))
# 
#####Set number of periods#####
Q = 1000

#####Set number of scenarios (including baselines)#####
S=3

#####Set period in which exogenous shift will occur#####
q=50

#####Create (S x Q) matrices in which equilibrium solutions from different parameterizations will be stored#####
u=matrix(data=0,nrow=S,ncol=Q) # rate of capacity utilization
g=matrix(data=0,nrow=S,ncol=Q)# growth rate
gk=matrix(data=0,nrow=S,ncol=Q)# growth rate of capital stock

#####Set constant parameter values (exógenos)#####
d=0.1   # time increment
s=0.1 # marginal propensity to save
v=4 # normal capital-output ratio
a=0.001 # reaction parameter

#####Set and initialize exogenous variables/parameters that will be shifted#####
g=matrix(data=s/v,nrow=S,ncol=Q) # growth rate
gk=matrix(data=s/v,nrow=S,ncol=Q)  # growth rate of capital stock
u=matrix(data=1,nrow=S,ncol=Q)  # rate of capacity utilization

#####Initialize endogenous variables at equilibrium values#####
g[,1]=s/v
gk[,1]=g[,1]
u[,1]=1

#####Simulate the model by looping over Q time periods for S different scenarios#####
for (i in 1:S){
  
  for (t in 2:Q){
    
    for (iterations in 1:5000){ # iterate the model 1000-times in each period
      
      #####Set parameter values for different scenarios#####
      g[2,q]=0.03 # Scenario 2: rise in growth rate above warranted rate
      g[3,q]=0.02 # Scenario 3: reduction in growth rate below warranted rate
      
      #(1) Growth rate
      g[i,t] = g[i,t-1] + a*(u[i,t-1]-1) #(SERRANO, FREITAS E BHERING 2018 - EQ. A6)
      
      #(1) Investment rate
      gk[i,t] = (s/v)*u[i,t-1] #(SERRANO, FREITAS E BHERING 2018 - EQ. A3 - slightly modified)
      
      #(2) Capacity utilization
      u[i,t] =  u[i,t-1]*((1+ g[i,t])/(1+gk[i,t])) #(SERRANO, FREITAS E BHERING 2018 - EQ. A7)
      
    } # close iterations loop
  }  # close time loop
}   # close scenarios loop

####BETTER PLOTS
# Set maximum period for plots


Tmax <- 200

####  G ####
# Extrair apenas os dados de tempo e transpor para criar data frame
plot_df <- as.data.frame(t(g[, 2:(Tmax+1)]))
colnames(plot_df) <- c("Taxa Garantida (2,5%)", "Elevação (3,0%)", "Redução (2,0%)")
plot_df$Time <- 1:Tmax

# Converter para formato longo
plot_long <- melt(plot_df, id.vars = "Time", variable.name = "Scenario", value.name = "GrowthRate")

# Criar o gráfico
plot_harrod_g <- ggplot(plot_long, aes(x = Time, y = GrowthRate, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Taxa Garantida (2,5%)" = "steelblue", "Elevação (3,0%)" = "forestgreen", "Redução (2,0%)" = "firebrick")) +
  scale_linetype_manual(values = c("Taxa Garantida (2,5%)" = "solid", "Elevação (3,0%)" = "dashed", "Redução (2,0%)" = "dashed")) +
  labs(x = "Períodos",
       y = "Taxa de Crescimento do Produto (g)") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

plot_harrod_g


ggsave(filename = paste0(".\\Output\\plot_harrod_g.png"), plot = plot_harrod_g, width = 16, height = 8, dpi = 800)
ggsave(filename = paste0(".\\Output\\plot_harrod_g.svg"), plot = plot_harrod_g, width = 16, height = 8, dpi = 800)

####  Gk ####
# Extrair apenas os dados de tempo e transpor para criar data frame
plot_df <- as.data.frame(t(gk[, 2:(Tmax+1)]))
colnames(plot_df) <- c("Taxa Garantida (2,5%)", "Elevação (3,0%)", "Redução (2,0%)")
plot_df$Time <- 1:Tmax

# Converter para formato longo
plot_long <- melt(plot_df, id.vars = "Time", variable.name = "Scenario", value.name = "GrowthRate")

# Criar o gráfico
plot_harrod_gk <- ggplot(plot_long, aes(x = Time, y = GrowthRate, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Taxa Garantida (2,5%)" = "steelblue", "Elevação (3,0%)" = "forestgreen", "Redução (2,0%)" = "firebrick")) +
  scale_linetype_manual(values = c("Taxa Garantida (2,5%)" = "solid", "Elevação (3,0%)" = "dashed", "Redução (2,0%)" = "dashed")) +
  labs(x = "Períodos",
       y = "Taxa de Crescimento do Estoque de Capital (gk)") +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 18),       # aumenta todas as fontes
    axis.title = element_text(size = 18), # títulos dos eixos
    axis.text = element_text(size = 16),  # valores dos eixos
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

plot_harrod_gk

ggsave(filename = paste0(".\\Output\\plot_harrod_gk.png"), plot = plot_harrod_gk, width = 16, height = 8, dpi = 800)
ggsave(filename = paste0(".\\Output\\plot_harrod_gk.svg"), plot = plot_harrod_gk, width = 16, height = 8, dpi = 800)


####  u ####

# # Extrair apenas os dados de tempo e transpor para criar data frame
# plot_df <- as.data.frame(t(u[, 2:(Tmax+1)]))
# colnames(plot_df) <- c("Taxa Garantida (2,5%)", "Elevação (3,0%)", "Redução (2,0%)")
# plot_df$Time <- 1:Tmax
# 
# # Converter para formato longo
# plot_long <- melt(plot_df, id.vars = "Time", variable.name = "Scenario", value.name = "GrowthRate")
# 
# # Criar o gráfico
# ggplot(plot_long, aes(x = Time, y = GrowthRate, color = Scenario, linetype = Scenario)) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("Taxa Garantida (2,5%)" = "steelblue", "Elevação (3,0%)" = "forestgreen", "Redução (2,0%)" = "firebrick")) +
#   scale_linetype_manual(values = c("Taxa Garantida (2,5%)" = "solid", "Elevação (3,0%)" = "dashed", "Redução (2,0%)" = "dashed")) +
#   labs(x = "Períodos",
#        y = "Grau de Utilização da Capacidade Instalada (u)") +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(size = 12, hjust = 0.5),
#     legend.title = element_blank(),
#     legend.position = "bottom"
  # )
