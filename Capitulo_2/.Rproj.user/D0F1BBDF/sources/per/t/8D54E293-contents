#Exercício para reproduzir com o pacote sfcr o artigo:
#DI BUCCHIANICO, Stefano; GALLO, Ettore; LOFARO, Antonino.
#Debt-credit Flows and Stocks in a Supermultiplier Model with Two Autonomous Demand Components: Consequences for Growth.
#Review of Political Economy, [s. l.], v. 36, n. 5, p. 1894–1914, 2024. 

basemodel <- lista_modelos_baseline[["BUCCHIANICO_2024_a_BASELINE"]]

#####TRANSACTION-FLOW####
tfm_bucchianico <- sfcr_matrix(
  columns = c("Workers","Rentiers", "Firms_current", "Firms_capital", "Banks"),
  codes = c("w", "r", "fc", "fk", "b"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Wages", w = "+W", fc = "-W"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Profits", r = "+FT +Fb ", fc = "-(FT + Fb)"),
  c("Ch. Loans", w = "+(Lw - Lw[-1])",b = "-(Lw - Lw[-1])"),
  c("Interest on loans", w = "-i*Lw[-1]",b = "+i*Lw[-1]"),
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", b = "d(D)"),
  c("Ch. Equities", r = "-d(E)", fk = "+d(E)"),
)

sfcr_matrix_display(tfm_bucchianico)

sfcr_validate(tfm_bucchianico, basemodel, which = "tfm", rtol = TRUE, tol = 1e-3)

####Structure of the model (DAG)####

sfcr_dag_blocks_plot(eqs_bucchianico2024)
sfcr_dag_cycles_plot(eqs_bucchianico2024, size = 10)

# View(sfcr_get_blocks(basemodel))

####SANKEY'S DIAGRAM####
#é necessário rodar o arquivo "funtion_sfcr_sankey.R" que corrige um detalhe da função original
source("funtion_sfcr_sankey.R")
sfcr_sankey(tfm_bucchianico, basemodel, when = "start")

sfcr_sankey(tfm_bucchianico, basemodel, when = "end")


####MATRIZES_V2####
#####BALANCE SHEET####
bs_bucchianico <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r", "f", "b", "s"),
  c("Deposits", w = "+Dw", r = "+Dr", b = "-D"),
  c("Loans", w = "-Lw", b = "+Lw"),
  c("Equities", r = "+E", f = "-E"),
  c("Capital", f = "+K", s ="+K"),
  c("Net Worth", w = "Vw", r = "Vr", f = "+K-E", b = "Lw - D",s ="+K")
)

sfcr_matrix_display(bs_bucchianico, "bs")

sfcr_validate(bs_bucchianico, basemodel, "bs", rtol = TRUE, tol = 1e-3)

# write.csv2(modelo_tese_H, file = "modelo_tese_H.csv", fileEncoding = "latin1")


#####TRANSACTION-FLOW####
tfm_bucchianico <- sfcr_matrix(
  columns = c("Workers","Rentiers", "Firms_current", "Firms_capital", "Banks"),
  codes = c("w", "r", "fc", "fk", "b"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Wages", w = "+W", fc = "-W"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Firm Profits", r = "+FD", fc = "-(FD+FU)", fk = "FU"),
  c("Bank Profits", r = "+Fb", b = "-Fb"),
  c("Interest on loans", w = "-i*Lw[-1]",b = "+i*Lw[-1]"),
  c("Workers' amortization", w = "-r * Lw[-1]", b ="+r * Lw[-1]"),
  c("Workers' new loans", w = "+Bw", b ="-Bw"),
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", b = "d(D)"),
  c("Ch. Equities", r = "-d(E)", fk = "+d(E)"),
)
sfcr_matrix_display(tfm_bucchianico)

sfcr_validate(tfm_bucchianico, basemodel, which = "tfm", rtol = TRUE, tol = 1e-3)

####Structure of the model (DAG)####

sfcr_dag_blocks_plot(eqs_bucchianico2024)
sfcr_dag_cycles_plot(eqs_bucchianico2024, size = 10)

View(sfcr_get_blocks(basemodel))

####SANKEY'S DIAGRAM####
sfcr_sankey(tfm_bucchianico, basemodel, when = "start")

sfcr_sankey(tfm_bucchianico, basemodel, when = "end")
