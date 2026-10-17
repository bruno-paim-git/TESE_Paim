# OBJETIVO:
# Calcular Juros e Amortização para cada modalidade
# exportar dados de saldo, concessões, taxa de juros, pagamentos ( amortização e juros) por modalidade
# as séries "_bcb" referem-se às originais
# as séries "_bru" referem-se à aplicação da metodologia proposta

#Para agregar as variáveis semelhantes:
#no cálculo do ICC ponderamos pelo saldo da carteira
#no cálculo dos juros ponderamos pelas concessões, diferente do BCB que pondera também pelo saldo
#optamos desta forma, pois é justamente por causa do efeito diferente sobre o ICC e sobre os Juros
#que as concessões gradualmente impactam no saldo

#define data do arquivo base
date_file <- "2026-09-09" 
{
####BIBLIOTECA####
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(vars)
library(svars)
library(TTR)
library(tsibble)
library(conflicted)
library(plotly)
library(seasonal)
library(webshot)
library(RCurl)
library(slider)
library(patchwork)
library(tseries)
library(urca)

options(scipen = 999)

conflict_prefer("layout", "plotly")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source("./function_export_plot.R")

}

bcb_pf_df <- readr::read_rds(file = paste0("./Output/bcb_pf_df_", date_file, ".rds"))

lista_resultados <- split(bcb_pf_df, bcb_pf_df$modalidade)

df_cartao_de_credito <- readr::read_rds(file = paste0("./Output/df_cartao_de_credito_", date_file, ".rds"))

#####MODALIDADES - PADRÃO#####
#Cálculo Comprometimento padrão
modalidades_padrao <- c(
  "Aquisição de outros bens",
  "Aquisição de veículos",
  "Arrendamento mercantil de outros bens",
  "Arrendamento mercantil de veículos",
  "Cartão de crédito parcelado",
  "Crédito pessoal consignado para aposentados e pensionistas do INSS",
  "Crédito pessoal consignado para trabalhadores do setor privado",
  "Crédito pessoal consignado para trabalhadores do setor público",
  "Crédito pessoal não consignado vinculado à composição de dívidas",
  "Crédito pessoal não consignado",
  "Crédito rural com taxas de mercado",
  "Crédito rural com taxas reguladas",
  # "Capital de giro com recursos do BNDES",
  "Financiamento de investimentos com recursos do BNDES",
  "Financiamento agroindustrial com recursos do BNDES",
  "Microcrédito destinado a consumo",
  "Microcrédito destinado a microempreendedores"
  # "Outros créditos livres",
  # "Outros créditos direcionados"
)


# Função para aplicar as transformações nos dataframes
calculo_comprometimento_padrao <- function(df) {
  df <- df %>%
    mutate_if(is.character, ~ type.convert(.x, as.is = TRUE)) %>%
    mutate(
      saldo_bcb = saldo,
      saldo_bru = saldo,
      concessoes_bcb = concessoes,
      concessoes_bru = concessoes,
      icc_am = ((((icc_aa / 100) + 1)^(1 / 12)) - 1) * 100,
      PMT = ((icc_am / 100) * saldo_bru) / (1 - (1 + icc_am / 100)^-(2 * prazo_cart - 1)),
      AMORTIZACAO = saldo_bru / (2 * prazo_cart - 1),
      JUROS = PMT - AMORTIZACAO,
    )
  return(df)
}
# Aplique a função a todos os dataframes da lista, passando a modalidade
df_list <- lapply(names(lista_resultados), function(modalidade) {
  # Verificar se a modalidade está em modalidades_padrao
  if (modalidade %in% modalidades_padrao && !is.null(lista_resultados[[modalidade]])) {
    df <- lista_resultados[[modalidade]]
    
    # Aplica o cálculo de comprometimento
    df <- calculo_comprometimento_padrao(df)
    
    # Manter o nome da modalidade na lista de resultados
    return(setNames(list(df), modalidade))
  }
  return(NULL)  # Retornar NULL se a modalidade não estiver em modalidades_padrao ou dataframe ausente
})

######RESULTADO#####
# Unir os resultados em uma lista final, mantendo os nomes das modalidades
lista_resultados_padrao <- do.call(c, df_list)




#####MODALIDADE - CC A VISTA####

######Cálculo PMT, Juros e Amortização#####
# desconta do saldo e das concessões os pagamento em 1 uma parcela
# para o cálculo da amortização, aplica o prazo médio ponderado calculado anteriormente

cc_parcelado_sem_juros <- lista_resultados[["Cartão de crédito à vista"]] |>
  left_join(df_cartao_de_credito) |> 
  mutate_if(is.character, ~ type.convert(.x, as.is = TRUE)) %>%
  mutate(
    concessoes_bcb = concessoes,
    concessoes_parcela_unica = perc_1_parc*concessoes,
    concessoes_bru = concessoes - concessoes_parcela_unica,
    saldo_bcb = saldo,
    saldo_bru = saldo - concessoes_parcela_unica,
    AMORTIZACAO = saldo_bru / prazo_medio ,
    JUROS = 0,
    PMT = AMORTIZACAO + JUROS
    ) |>
  select(date, saldo, saldo_bcb, saldo_bru, concessoes, concessoes_bcb, concessoes_bru, AMORTIZACAO, PMT)


#####MODALIDADE - CC ROTATIVO####
#utiliza o prazo médio de 6,7
#nota de rodapé da tabela EMC: 2/ Nos dados de concessões não estão inclusas as modalidades cartão de crédito parcelado e rotativo.

cc_rotativo <- lista_resultados[["Cartão de crédito rotativo"]] %>%
  mutate_if(is.character, ~ type.convert(.x, as.is = TRUE)) %>%
  mutate(
    concessoes_bcb = concessoes,
    concessoes_bru = concessoes,
    saldo_bcb = saldo,
    saldo_bru = saldo,
    icc_am = ((((tx_juros / 100) + 1)^(1 / 12)) - 1) * 100,
    AMORTIZACAO = saldo_bru/ 6.7,
    JUROS = saldo_bru*icc_am/100,
    PMT = AMORTIZACAO + JUROS
    )

#####MODALIDADE - CHEQUE ESPECIAL####

### A seguinte alteração não foi realizada, pois a taxa sobre o limite não utilizada não foi aplicada pelas instituições financeiras.
#De qualquer modo, optou-se por manter aqui a ideia: 
#considera-se que, a partir de jan/2020, há uma elevação de 3,125p.p. no custo do crédito acima do indicado
#isto porque passou-se a permitir a cobrança de 0,25% sobre o limite do cartão de crédito mesmo quando não utilizado
#chegou-se ao valor aproximado de 3p.p. pois a taxa de utilização da modalidade é cerca de 8%
#como só temos as concessões de quem efetivamente utiliza, dividiu-se 0,25 por 0,08, chegando a 3,125

#Para cálculo da amortização, dada a característica de alta recorrência vis-à-vis duração inferior a um mês, 
#utilizou-se o prazo médio de 12 meses para amortização, 
#com base na distribuição de frequência de uso do Cheque Especial (BCB, 2019, p.17). 

cheque_especial <- lista_resultados[["Cheque especial"]] %>%
  mutate_if(is.character, ~ type.convert(.x, as.is = TRUE)) %>%
  arrange(date) |> 
  mutate(
    concessoes_bcb = concessoes,
    # concessoes_bru = concessoes,
    saldo_bcb = saldo,
    saldo_bru = saldo,
    icc_am = ((((tx_juros / 100) + 1)^(1 / 12)) - 1) * 100,
    # icc_am_ajust = if_else(date < "2020-01-01", icc_am, icc_am+3.125),
    AMORTIZACAO = (saldo_bru/prazo_cart)/12,
    JUROS = (saldo_bru/prazo_cart)*icc_am/100,
    PMT = AMORTIZACAO + JUROS,
    concessoes_bru = saldo_bru - lag(saldo_bru,1) + PMT
    )

#####MODALIDADE - HABITACIONAL####

modalidade_habitacional <- c("Financiamento imobiliário com taxas de mercado",
                             "Financiamento imobiliário com taxas reguladas")
# Função para aplicar as transformações nos dataframes
calculo_comprometimento_habitacional <- function(df) {
  df <- df %>%
    mutate_if(is.character, ~ type.convert(.x, as.is = TRUE)) %>%
    mutate(
      concessoes_bcb = concessoes,
      concessoes_bru = concessoes,
      saldo_bcb = saldo,
      saldo_bru = saldo,
      icc_am = ((((icc_aa / 100) + 1)^(1 / 12)) - 1) * 100,
      AMORTIZACAO = saldo_bru / (2 * prazo_cart - 1),
      JUROS = saldo_bru*(icc_am/100),
      PMT = AMORTIZACAO + JUROS
    )
  return(df)
}

# Aplique a função a todos os dataframes da lista, passando a modalidade
df_list <- lapply(names(lista_resultados), function(modalidade) {
  # Verificar se a modalidade está em modalidades_padrao
  if (modalidade %in% modalidade_habitacional && !is.null(lista_resultados[[modalidade]])) {
    df <- lista_resultados[[modalidade]]
    
    # Aplica o cálculo de comprometimento
    df <- calculo_comprometimento_habitacional(df)
    
    # Manter o nome da modalidade na lista de resultados
    return(setNames(list(df), modalidade))
  }
  return(NULL)  # Retornar NULL se a modalidade não estiver em modalidades_padrao ou dataframe ausente
})

# Unir os resultados em uma lista final, mantendo os nomes das modalidades
lista_resultados_habitacional <- do.call(c, df_list)


#####PMT TODAS AS MODALIDADE####
lista_completa <- c(lista_resultados_padrao,  
                    lista_resultados_habitacional,
                    list("Cartão de crédito parcelado sem juros" = cc_parcelado_sem_juros ,
                         "Cartão de crédito rotativo" = cc_rotativo,
                         "Cheque especial" = cheque_especial))

# Usando imap para adicionar uma coluna com o nome de cada dataframe
lista_com_nome <- imap(lista_completa, ~ mutate(.x, modalidade = .y))

# Empilhando todos os dataframes da lista e criando a coluna 'nome_lista'
df_empilhado <- bind_rows(lista_com_nome)



modalidades_selecionadas <- c(
  "Aquisição de outros bens",
  "Aquisição de veículos",
  "Arrendamento mercantil de outros bens",
  "Arrendamento mercantil de veículos",
  "Cartão de crédito parcelado sem juros",
  "Cartão de crédito parcelado",
  "Cartão de crédito rotativo",
  "Cheque especial",
  "Crédito pessoal consignado para aposentados e pensionistas do INSS",
  "Crédito pessoal consignado para trabalhadores do setor privado",
  "Crédito pessoal consignado para trabalhadores do setor público", 
  "Crédito pessoal não consignado",
  "Crédito pessoal não consignado vinculado à composição de dívidas",
  "Financiamento imobiliário com taxas de mercado",
  "Financiamento imobiliário com taxas reguladas"
)

unique(df_empilhado$modalidade)

tabela_10 <- df_empilhado |> 
  filter(modalidade %in% modalidades_selecionadas) |> 
  mutate(ano = year(date)) |> 
  filter(ano %in% c(2013, 2025)) |>  
  group_by(ano, modalidade) |> 
  summarise(
    saldo_total = sum(saldo, na.rm = TRUE),
    concessoes_total = sum(concessoes, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(ano) |> 
  mutate(
    perc_saldo = sprintf("%.1f", round(saldo_total / sum(saldo_total) * 100, 1)),
    perc_concessoes = sprintf("%.1f", round(concessoes_total / sum(concessoes_total) * 100, 1))
  ) |> 
  ungroup() |>
  select(-saldo_total, -concessoes_total) |>
  pivot_wider(names_from = ano, values_from = c(perc_saldo, perc_concessoes))


tabela_10
readr::write_excel_csv2( tabela_10,
                         file = paste0("./Output/tabela_10_", Sys.Date(),".csv"))

#####save_the_data####

readr::write_rds(df_empilhado,
                 file = paste0("./Output/df_empilhado_",  date_file, ".rds"))

readr::write_excel_csv2( df_empilhado,
                         file = paste0("./Output/df_empilhado_", date_file, ".csv"))

