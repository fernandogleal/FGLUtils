#' Fetch exchange rate data from IPEA
#'
#' The `dados_dolar` function fetches exchange rate data of Brazilian Real against US Dollar from IPEA. The function uses the OData API from IPEA to retrieve the data and calculates the changes in prices. The final result is a data frame with two columns: `Data` and `valor`. The `Data` column represents the date, and the `valor` column represents the exchange rate in Brazilian Real against US Dollar. The function returns a data frame and prints a message indicating the type of data returned.
#'
#' @return A data frame with two columns: `Data` and `valor`.
#'
#' @references Os índeces podem ser acessados através do acesso ao sítio do IPEA Data
#' \url{http://www.ipeadata.gov.br/}.
#'
#' @return A \code{tibble}, com os valor do dólar
#'
#' @examples
#' dolar <- get_dollar_data()
#'
#' @export
get_dollar_data <- function(){

  tmp <- tempfile()

  dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='GM366_ERC366')")
  httr::stop_for_status(dados, task = "Buscando os dados do dolar da API do IPEA.")

  # Calculate changes in prices
  dados <- httr::content(dados)[[2]]
  dados <- dplyr::bind_rows(dados)[,2:3]
  dados$VALDATA <- lubridate::as_date(dados$VALDATA)
  dados <- dados |>
    dplyr::select(Data := 1, valor:= 2) |>
    dplyr::filter(!is.na(valor))

  message("Taxa de cambio - R$/US$ comercial/compra - media")

  return(dados)

}


