
####SHOCKS####

base_model <- lista_modelos_baseline[["BETA135_PHI06"]]

####Choque em i####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.1,        
  ),
  start = 50,
  end = period_simulation
)

list_CHOQUE <- c(0.0818, 0.0918) #CHOQUE i
shock_exp <- sfcr_expand(shock_fix, i, list_CHOQUE)
tese_shock <- sfcr_multis(shock_exp, base_model, periods = 1000, method = "Newton")
names(tese_shock) <- list_CHOQUE

df_scenario1_TOTAL <- map_df(tese_shock, process_model_df, .id = "CHOQUE")

var_shock <- "01_shock_i_"

salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"))
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"))
salvar_subplot(c("dsbw_y", "dszw_y", "dsw_y"), filename =  paste0(var_shock,"C"))
salvar_subplot(c("cw_y", "cr_y"), filename =  paste0(var_shock,"D"))


####Choque em gZ####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    phi0 ~ 0.05        
  ),
  start = 50,
  end = period_simulation
)

list_CHOQUE <- c(0.06, 0.05) #CHOQUE gZ
shock_exp <- sfcr_expand(shock_fix, phi0, list_CHOQUE)
tese_shock <- sfcr_multis(shock_exp, base_model, periods = 1000, method = "Newton")
names(tese_shock) <- list_CHOQUE

df_scenario1_TOTAL <- map_df(tese_shock, process_model_df, .id = "CHOQUE")

var_shock <- "02_shock_phi0_"
salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"))
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"))
salvar_subplot(c("dsbw_y", "dszw_y", "dsw_y"), filename =  paste0(var_shock,"C"))
salvar_subplot(c("cw_y", "cr_y"), filename =  paste0(var_shock,"D"))


####Choque em beta####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    beta ~ 0.7      
  ),
  start = 50,
  end = period_simulation
)

list_CHOQUE <- c(0.135, 0.25)  #CHOQUE beta
shock_exp <- sfcr_expand(shock_fix, beta, list_CHOQUE)
tese_shock <- sfcr_multis(shock_exp, base_model, periods = 1000, method = "Newton")
names(tese_shock) <- list_CHOQUE

df_scenario1_TOTAL <- map_df(tese_shock, process_model_df, .id = "CHOQUE")

var_shock <- "03_shock_beta_"
salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"))
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"))
salvar_subplot(c("dsbw_y", "dszw_y", "dsw_y"), filename =  paste0(var_shock,"C"))
salvar_subplot(c("cw_y", "cr_y"), filename =  paste0(var_shock,"D"))
salvar_subplot(c("lbw", "lzw", "lw"), filename =  paste0(var_shock,"E"))

