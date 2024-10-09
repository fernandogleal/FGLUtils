#' Calcular o salario liquido
#'
#' @description
#' Calcular o salario líquido de impostos
#'
#' @param funcionario Nome do funcionario
#' @param proventos Total da receita tributada
#' @param dependentes Numero de dependentes
#' @param adiantamento Valor recebido de adiantamento
#' @param pgbl Eventuais valor aportado em fundo PGBL
#'
#' @return A \code{tibble}, os dados do salario, deduções e salario liquido
#'
#' @examples
#' # FGL <- calculate_net_salary(funcionario = "FGL", proventos = 84726.72, dependentes=2, adiantamento = 0, pgbl = 21827.08)
#'
#' @export
calculate_net_salary <- function (funcionario = "FGL", proventos, dependentes = 0, adiantamento = 0, pgbl = 0)
{
  # Parâmetros dedução de dependente
  deducao_dependente <- 189.59

  # Parâmetros IRRF
  IRRF_faixa1 <- 2259.2
  IRRF_faixa2 <- 2826.65
  IRRF_faixa3 <- 3751.05
  IRRF_faixa4 <- 4664.68
  IRRF_parcela_dedutivel_faixa2 <- 169.44
  IRRF_parcela_dedutivel_faixa3 <- 381.44
  IRRF_parcela_dedutivel_faixa4 <- 662.77
  IRRF_parcela_dedutivel_faixa5 <- 896


  ## I N S S
  INSS_faixa1 <- 1412.00
  INSS_faixa2 <- 2666.68
  INSS_faixa3 <- 4000.03
  INSS_teto <- 7786.02

  aliquota1 <- 0.075
  aliquota2 <- 0.09
  aliquota3 <- 0.12
  aliquota4 <- 0.14

  if (proventos <= INSS_faixa1) {
    inss <- proventos * aliquota1
  } else if (proventos <= INSS_faixa2) {
    inss <- (INSS_faixa1 * aliquota1) + ((proventos - INSS_faixa1) * aliquota2)
  } else if (proventos <= INSS_faixa3) {
    inss <- (INSS_faixa1 * aliquota1) + ((INSS_faixa2 - INSS_faixa1) * aliquota2) + ((proventos - INSS_faixa2) * aliquota3)
  } else if (proventos <= INSS_teto) {
    inss <- (INSS_faixa1 * aliquota1) + ((INSS_faixa2 - INSS_faixa1) * aliquota2) + ((INSS_faixa3 - INSS_faixa2) * aliquota3) + ((proventos - INSS_faixa3) * aliquota4)
  } else {
    inss <- (INSS_faixa1 * aliquota1) + ((INSS_faixa2 - INSS_faixa1) * aliquota2) + ((INSS_faixa3 - INSS_faixa2) * aliquota3) + ((INSS_teto - INSS_faixa3) * aliquota4)
  }


  # F G T S
  fgts <- proventos * 0.08
  if (pgbl > proventos * 0.12) {
    pgbl_dedutivel <- proventos * 0.12
    pgbl_nao_dedutivel <- pgbl - pgbl_dedutivel
  }
  else {
    pgbl_dedutivel <- pgbl
    pgbl_nao_dedutivel <- 0
  }

  deducao_dependentes <- dependentes * deducao_dependente

  base_calculo <- proventos - inss - deducao_dependentes - pgbl

  # pgbl_dedutivel
  if (base_calculo <= IRRF_faixa1) {
    irrf <- 0
  }
  else if (base_calculo <= IRRF_faixa2) {
    irrf <- (base_calculo * 0.075) - IRRF_parcela_dedutivel_faixa2
  }
  else if (base_calculo <= IRRF_faixa3) {
    irrf <- (base_calculo * 0.15) - IRRF_parcela_dedutivel_faixa3
  }
  else if (base_calculo <= IRRF_faixa4) {
    irrf <- (base_calculo * 0.225) - IRRF_parcela_dedutivel_faixa4
  }
  else {
    irrf <- (base_calculo * 0.275) - IRRF_parcela_dedutivel_faixa5
  }
  proventos_liquido <- proventos - inss - irrf - adiantamento
  df_resultado <- dplyr::as_tibble(data.frame(funcionario,
                                              funcionario, proventos = proventos, adiantamento = adiantamento,
                                              pgbl_dedutivel = pgbl_dedutivel, pgbl_nao_dedutivel = pgbl_nao_dedutivel,
                                              dependentes = deducao_dependentes, inss = inss, irrf = irrf,
                                              fgts = fgts, proventos_liquido = proventos_liquido))
  return(df_resultado)
}
