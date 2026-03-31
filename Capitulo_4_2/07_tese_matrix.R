nome_modelo <- "BETA135_PHI06"
beta_v <- 0.135
phi0_v <- 0.06
period_simulation <- 200


base_model <- sfcr_scenarios(nome_modelo,beta_v, phi0_v , selected_equations = eqs_K, period_simulation)



####MATRIZES####
#####BALANCE SHEET####
bs_tese_K <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r","f", "b", "s"),
  c("Capital Goods", f = "+K", s ="+K"),
  c("Deposits", w = "+Dw", r = "+Dr", b = "-D"),
  c("Loans", w = "-Lw", b = "+Lw"),
  c("Equities", r = "+E", f = "-E"),
  c("Net Worth", w = "Vw", r = "Vr", f = "+K-E", b = "Lw - D",s ="+K")
)

sfcr_matrix_display(bs_tese_K, "bs")
sfcr_validate(bs_tese_K, base_model, "bs", rtol = TRUE, tol = 1e-3)


#####TRANSACTION-FLOW####
tfm_tese_K <- sfcr_matrix(
  columns = c("Workers","Rentiers", "Firms_current", "Firms_capital", "Banks"),
  codes = c("w", "r", "fc", "fk", "b"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Wages", w = "+W", fc = "-W"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Firm Profits", r = "+FD", fc = "-(FD+FU)", fk = "FU"),
  c("Bank Profits", r = "+Fb", b = "-Fb"),
  c("Workers' new loans", w = "+Bw +Zw", b ="-Bw -Zw"),
  c("Loan Interest", w = "-i*(lambda_b*Lbw[-1] + lambda_z*Lzw[-1])",b = "+i*(lambda_b*Lbw[-1] + lambda_z*Lzw[-1])"),
  c("Workers' amortization", w = "-(r_b * Lbw[-1] + r_z * Lzw[-1])", b ="+(r_b * Lbw[-1] + r_z * Lzw[-1])"),
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", b = "+d(Dw)+d(Dr)"),
  c("Ch. Equities", r = "-d(E)", fk = "+d(E)"),
)

sfcr_matrix_display(tfm_tese_K)
sfcr_validate(tfm_tese_K, base_model, which = "tfm", rtol = TRUE, tol = 1e-3)

####Structure of the model (DAG)####

plot_dag_tese <- sfcr_dag_blocks_plot(eqs_K)
plot_dag_tese

ggsave(filename = paste0(".\\Output\\plot_dag_tese.png"), 
       plot = plot_dag_tese, width = 12, height = 8, dpi = 1000)

sfcr_dag_cycles_plot(eqs_K, size = 10)

# View(sfcr_get_blocks(base_model))

####SANKEY'S DIAGRAM####
source("06_function_sfcr_sankey.R")
sfcr_sankey(tfm_tese_K, base_model, when = "start")

plot_sankey_end <- sfcr_sankey(tfm_tese_K, base_model, when = "end")
plot_sankey_end


# 1. Salvar o gráfico como HTML temporário
htmlwidgets::saveWidget(plot_sankey_end, "temp_plot.html")

# 2. Capturar imagem a partir do HTML
webshot("temp_plot.html", file = paste0(".\\Output\\plot_sankey_end.png"), vwidth = 1000, vheight = 700)

