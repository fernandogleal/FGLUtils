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
  if (proventos <= 1320) {
    inss <- proventos * 0.075
  }
  else if (proventos <= 2571.29) {
    inss <- proventos * 0.09 - 1302 * 0.09
  }
  else if (proventos <= 3856.94) {
    inss <- proventos * 0.12 - 3856.94 * 0.12
  }
  else if (proventos <= 7507.94) {
    inss <- proventos * 0.14 - 7507.94 * 0.14
  }
  else {
    inss <- 877.22
  }
  fgts <- proventos * 0.08
  if (pgbl > proventos * 0.12) {
    pgbl_dedutivel <- proventos * 0.12
    pgbl_nao_dedutivel <- pgbl - pgbl_dedutivel
  }
  else {
    pgbl_dedutivel <- pgbl
    pgbl_nao_dedutivel <- 0
  }
  deducao_dependentes <- dependentes * 189.59
  base_calculo <- proventos - inss - deducao_dependentes - pgbl
  # pgbl_dedutivel
  if (base_calculo <= 2112) {
    irrf <- 0
  }
  else if (base_calculo <= 2826.65) {
    irrf <- (base_calculo * 0.075) - 158.4
  }
  else if (base_calculo <= 3751.05) {
    irrf <- (base_calculo * 0.15) - 370.4
  }
  else if (base_calculo <= 4664.68) {
    irrf <- (base_calculo * 0.225) - 651.73
  }
  else {
    irrf <- (base_calculo * 0.275) - 884.96
  }
  proventos_liquido <- proventos - inss - irrf - adiantamento
  df_resultado <- dplyr::as_tibble(data.frame(funcionario,
                                              funcionario, proventos = proventos, adiantamento = adiantamento,
                                              pgbl_dedutivel = pgbl_dedutivel, pgbl_nao_dedutivel = pgbl_nao_dedutivel,
                                              dependentes = deducao_dependentes, inss = inss, irrf = irrf,
                                              fgts = fgts, proventos_liquido = proventos_liquido))
  return(df_resultado)
}
