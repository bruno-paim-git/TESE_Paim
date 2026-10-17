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
  
  source("./07_function_sfcr_sankey.R")
}



nome_modelo <- "BETA03_PHI06"
beta_v <- 0.03
phi0_v <- 0.06
period_simulation <- 300


base_model <- sfcr_scenarios(nome_modelo,beta_v, phi0_v , selected_equations = eqs_K, period_simulation)



####MATRIZES####
#####BALANCE SHEET####
bs_tese_K <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r","f", "b", "s"),
  c("Fixed Capital",             f = "+K", s ="+K"),
  c("Deposits",       w = "+Dw", r = "+Dr", b = "-D"),
  c("Loans", w = "-L", b = "+L"),
  c("Equities", r = "+E", f = "-E"),
  
  c("Net Worth", w = "Vw", r = "Vr", f = "+K - E",s ="+K")
  
)

sfcr_matrix_display(bs_tese_K, "bs")
sfcr_validate(bs_tese_K, base_model, "bs", rtol = TRUE, tol = 1e-8)


#####TRANSACTION-FLOW####
tfm_tese_K <- sfcr_matrix(
  columns = c("Workers","Rentiers", "Firms_current", "Firms_capital", "Banks_current", "Banks_capital"),
  codes = c("w", "r", "fc", "fk", "bc", "bk"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Wages", w = "+W", fc = "-W"),
  c("Profits", r = "FD +Fb",  fc = "-FD", bc = "-Fb"),
  c("Interest on Loans", w = "-(icc_b*Lb[-1] + icc_z*Lz[-1])", bc = "+(icc_b*Lb[-1] + icc_z*Lz[-1])"), 
  c("Loan amortization", w = "-(r_b * Lb[-1] + r_z * Lz[-1])", bk ="+(r_b * Lb[-1] + r_z * Lz[-1])"), #V1
  c("New Loans", w = "+Bw+Zw", bk = "-Bw-Zw"), #V1
  # c("Ch. Loans", w = "+d(Lw)", b = "-d(Lw)"),#V2
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", bk = "+d(D)"),
  c("Ch. Equities", r = "-d(E)", fk = "+d(E)")
)

sfcr_matrix_display(tfm_tese_K)
sfcr_validate(tfm_tese_K, base_model, which = "tfm", rtol = TRUE, tol = 1e-8)

####Structure of the model (DAG)####

plot_dag_tese <- sfcr_dag_blocks_plot(eqs_K)
plot_dag_tese

ggsave(filename = paste0(".\\Output\\plot_dag_tese.png"), 
       plot = plot_dag_tese, width = 12, height = 8, dpi = 1000)

plot_dag_tese_2 <- sfcr_dag_cycles_plot(eqs_K, size = 10)

plot_dag_tese_2

ggsave(filename = paste0(".\\Output\\plot_dag_tese_2.png"), 
       plot = plot_dag_tese_2, width = 12, height = 8, dpi = 1000)

# View(sfcr_get_blocks(base_model))

####SANKEY'S DIAGRAM####
source("07_function_sfcr_sankey.R")
plot_sankey_start <- sfcr_sankey(tfm_tese_K, base_model, when = "start")
plot_sankey_start

plot_sankey_end <- sfcr_sankey(tfm_tese_K, base_model, when = "end")
plot_sankey_end

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(plot_sankey_end, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\plot_sankey_end.png"), vwidth = 1000, vheight = 700)

# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(plot_sankey_start, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\plot_sankey_start.png"), vwidth = 1000, vheight = 700)

