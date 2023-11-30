#' format_percent
#'
#' Esta função formata o percentual de acordo com o padrão Brasileiro.
#'
#' @param x O número a ser formatado.
#' @param acc A precisão desejada para o número formatado. O padrão é 0.1.
#' @param scale A escala do numero que esta sendo passado. 1 ou 100.
#'
#' @return Retorna o número formatado como uma string.
#' @export
#'
#' @examples
#' # formatar_numero(123456.789, acc = 0.01, scale = 100)
format_percent <- function(x, acc = 0.1, scale = 100){

  scales::percent(
    x,
    accuracy = acc,
    big.mark = ".",
    decimal.mark = ",",
    scale = scale
  )

}
