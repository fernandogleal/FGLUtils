#' Formatar Número
#'
#' Esta função formata um número de acordo com o padrão Brasileiro.
#'
#' @param x O número a ser formatado.
#' @param acc A precisão desejada para o número formatado. O padrão é 0.1.
#'
#' @return Retorna o número formatado como uma string.
#' @export
#'
#' @examples
#' # format_number(123456.789)
#' # format_number(123456.789, acc = 0.01)
format_number <- function(x, acc = 0.1){

  scales::number(
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ","
  )

}
