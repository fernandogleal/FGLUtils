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
#' # FGL <- calculate_net_salary(uncionario = "FGL", proventos = 10000, dependentes = 2)
#'
#' @export
calculate_net_salary <- function(funcionario = "FGL", proventos, dependentes, adiantamento = 0, pgbl = 0) {

  # Cálculo do INSS
  if (proventos <= 1302) {
    inss <- proventos * 0.075
  } else if (proventos <= 2571.29) {
    inss <- proventos * 0.09 - 1302*0.09
  } else if (proventos <= 3856.94) {
    inss <- proventos * 0.12 - 3856.94*0.12
  } else if (proventos <= 7507.94) {
    inss <- proventos * 0.14 - 7507.94*0.14
  } else {
    inss <- 877.22
  }

  # Cálculo do FGTS
  fgts <- proventos*0.08

  # Cálculo da dedutibilidade do PGBL
  if (pgbl > proventos * 0.12) {
    pgbl_dedutivel <- proventos * 0.12
    pgbl_nao_dedutivel <- pgbl - pgbl_dedutivel
  } else {
    pgbl_dedutivel <- pgbl
    pgbl_nao_dedutivel <- 0
  }

  deducao_dependentes <- dependentes * 189.59


  # Cálculo do IRRF
  base_calculo <- proventos - inss - deducao_dependentes - pgbl_dedutivel

  if (base_calculo <= 1903.98) {
    irrf <- 0
  } else if (base_calculo <= 2826.65) {
    irrf <- (base_calculo * 0.075) - 142.8
  } else if (base_calculo <= 3751.05) {
    irrf <- (base_calculo * 0.15) - 354.8
  } else if (base_calculo <= 4664.68) {
    irrf <- (base_calculo * 0.225) - 636.13
  } else {
    irrf <- (base_calculo * 0.275) - 869.36
  }

  # Cálculo do salário líquido
  proventos_liquido <- proventos - inss - irrf - adiantamento

  # Criação do dataframe com os resultados
  df_resultado <- data.frame(funcionario, funcionario,
                             proventos = proventos,
                             adiantamento = adiantamento,
                             pgbl_dedutivel = pgbl_dedutivel,
                             pgbl_nao_dedutivel = pgbl_nao_dedutivel,
                             dependentes = deducao_dependentes,
                             inss = inss,
                             irrf = irrf,
                             fgts = fgts,
                             proventos_liquido = proventos_liquido) |>
    dplyr::as_tibble()

  return(df_resultado)
}
