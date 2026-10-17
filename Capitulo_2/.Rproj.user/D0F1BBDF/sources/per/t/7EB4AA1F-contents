
####SHOCKS####
period_simulation <- 1000

base_model <- lista_modelos_baseline[["BETA03_PHI06"]]

####Choque em i####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    i ~ 0.04,        
  ),
  start = 50,
  end = period_simulation
)

list_CHOQUE <- c(0.05, 0.06) #CHOQUE i
shock_exp <- sfcr_expand(shock_fix, i, list_CHOQUE)
tese_shock <- sfcr_multis(shock_exp, base_model, periods = 1000, method = "Newton")
names(tese_shock) <- list_CHOQUE

df_scenario1_TOTAL <- map_df(tese_shock, process_model_df, .id = "CHOQUE")

var_shock <- "01_shock_i_"

horizon_periods = 500
gerar_subplot_ggplot(c("u", "h","dsb_y", "dsz_y", "ds_y","cw_y", "cr_y", "lb_w", "lz_w", "l_w", "ciw_y", "bw_y", "zw_y", "lb_y", "lz_y", "l_y", "icc_z", "icc_b"), periods = horizon_periods)

salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"), periods = horizon_periods)
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"), periods = horizon_periods)
salvar_subplot(c("dsb_w", "dsz_w", "ds_w"), filename =  paste0(var_shock,"C"), periods = horizon_periods)
salvar_subplot(c("ciw_y", "bw_y", "zw_y", "cw_y", "cr_y", "c_y"), filename =  paste0(var_shock,"D"), periods = horizon_periods)
salvar_subplot(c("lb_y", "lz_y", "l_y"), filename =  paste0(var_shock,"E"), periods = horizon_periods)
salvar_subplot(c("icc_z", "icc_b"), filename =  paste0(var_shock,"F"), periods = horizon_periods)


####Choque em gZ####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    phi0 ~ 0.06       
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
horizon_periods = 500

gerar_subplot_ggplot(c("u", "h","dsb_y", "dsz_y", "ds_y","cw_y", "cr_y", "lb_w", "lz_w", "l_w", "ciw_y", "bw_y", "zw_y", "lb_y", "lz_y", "l_y", "icc_z", "icc_b"), periods = horizon_periods)


salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"), periods = horizon_periods)
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"), periods = horizon_periods)
salvar_subplot(c("dsb_w", "dsz_w", "ds_w"), filename =  paste0(var_shock,"C"), periods = horizon_periods)
salvar_subplot(c("ciw_y", "bw_y", "zw_y", "cw_y", "cr_y", "c_y"), filename =  paste0(var_shock,"D"), periods = horizon_periods)
salvar_subplot(c("lb_y", "lz_y", "l_y"), filename =  paste0(var_shock,"E"), periods = horizon_periods)
salvar_subplot(c("icc_z", "icc_b"), filename =  paste0(var_shock,"F"), periods = horizon_periods)

####Choque em beta####
shock_fix <- sfcr_shock(
  variables = sfcr_set(
    beta ~ 0.2     
  ),
  start = 50,
  end = period_simulation
)

list_CHOQUE <- c(0.03, 0.06)  #CHOQUE beta
shock_exp <- sfcr_expand(shock_fix, beta, list_CHOQUE)
tese_shock <- sfcr_multis(shock_exp, base_model, periods = 1000, method = "Newton")
names(tese_shock) <- list_CHOQUE

df_scenario1_TOTAL <- map_df(tese_shock, process_model_df, .id = "CHOQUE")

var_shock <- "03_shock_beta_"
horizon_periods = 500

gerar_subplot_ggplot(c("u", "h","dsb_y", "dsz_y", "ds_y","cw_y", "cr_y", "lb_w", "lz_w", "l_w", "ciw_y", "bw_y", "zw_y", "lb_y", "lz_y", "l_y", "icc_z", "icc_b"), periods = horizon_periods)

salvar_subplot(c("u", "h"), filename =  paste0(var_shock,"A"), periods = horizon_periods)
salvar_subplot(c("log_y", "gZ"), filename =  paste0(var_shock,"B"), periods = horizon_periods)
salvar_subplot(c("dsb_w", "dsz_w", "ds_w"), filename =  paste0(var_shock,"C"), periods = horizon_periods)
salvar_subplot(c("ciw_y", "bw_y", "zw_y", "cw_y", "cr_y", "c_y"), filename =  paste0(var_shock,"D"), periods = horizon_periods)
salvar_subplot(c("lb_y", "lz_y", "l_y"), filename =  paste0(var_shock,"E"), periods = horizon_periods)
salvar_subplot(c("icc_z", "icc_b"), filename =  paste0(var_shock,"F"), periods = horizon_periods)

# gerar_subplot_ggplot(c("u", "h","dsbw_y", "dszw_y", "dsw_y","cw_y", "cr_y", "lbw", "lzw", "lw", "ciw_y", "bw_y", "zw_y", "lbw_y", "lzw_y", "lw_y"), periods = horizon_periods)

