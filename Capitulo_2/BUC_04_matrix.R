#Exercício para reproduzir com o pacote sfcr o artigo:
#DI BUCCHIANICO, Stefano; GALLO, Ettore; LOFARO, Antonino.
#Debt-credit Flows and Stocks in a Supermultiplier Model with Two Autonomous Demand Components: Consequences for Growth.
#Review of Political Economy, [s. l.], v. 36, n. 5, p. 1894–1914, 2024. 

basemodel <- lista_modelos_baseline[["BUCCHIANICO_2024_a_BASELINE"]]

####MATRIZES2####
#####BALANCE SHEET####
bs_bucchianico <- sfcr_matrix(
  columns = c("Workers", "Rentiers", "Firms", "Banks", "Sum"),
  codes = c("w", "r", "f", "b", "s"),
  c("Capital", f = "+K", s ="+K"),
  c("Deposits", w = "+Dw", r = "+Dr", b = "-D"),
  c("Loans", w = "-Lw", b = "+Lw"),
  c("Equities", r = "+E", f = "-E"),
  c("Net Worth", w = "Vw", r = "Vr", f = "+K-E", b = "Lw - D",s ="+K")
)

sfcr_matrix_display(bs_bucchianico, "bs")

sfcr_validate(bs_bucchianico, basemodel, "bs", rtol = TRUE, tol = 1e-8)

# write.csv2(modelo_tese_H, file = "modelo_tese_H.csv", fileEncoding = "latin1")


tfm_bucchianico <- sfcr_matrix(
  columns = c("Workers","Rentiers", "Firms_current", "Firms_capital", "Banks_current", "Banks_capital"),
  codes = c("w", "r", "fc", "fk", "bc", "bk"),
  c("Consumption", w = "-Cw", r = "-Cr", fc = "+C"),
  c("Wages", w = "+W", fc = "-W"),
  c("Investment", fc = "+I", fk = "-I"),
  c("Profits", r = "+FD + Fb", fc = "-FD", fk = "FU", bc = "-Fb"),
  c("Loan Flow", w = "(Bw - r*Lw[-1])", bk = "-(Bw - r*Lw[-1])"),
  c("Interest on loans", w = "-i*Lw[-1]", bc = "+i*Lw[-1]"),
  c("Ch. Loans", w = "-d(Lw)", bk = "d(Lw)"),
  c("Ch. Deposits", w = "-d(Dw)", r = "-d(Dr)", bk = "d(D)"),
  c("Ch. Equities", r = "-d(E)", fk = "+d(E)")
)
sfcr_matrix_display(tfm_bucchianico)

sfcr_validate(tfm_bucchianico, basemodel, which = "tfm", rtol = TRUE, tol = 1e-8)

####Structure of the model (DAG)####

sfcr_dag_blocks_plot(eqs_bucchianico2024)
sfcr_dag_cycles_plot(eqs_bucchianico2024, size = 10)

####SANKEY'S DIAGRAM####
sfcr_sankey(tfm_bucchianico, basemodel, when = "start")

sfcr_sankey(tfm_bucchianico, basemodel, when = "end")
