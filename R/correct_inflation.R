#' Reajusta um valor de acordo com a inflação
#'
#' @description
#' \code{correct_inflation()} usa dados do IPEADATA para reajustar um valor, de acordo com um índice.
#'
#' @param valor Valor a ser reajustado (default é 1000.00).
#' @param database Data base para o reajuste (default é "01/2020").
#' @param indice Indice de inflação a ser utilizado (default é "igpm"). Os índices disponíveis sã: ipca, igpm, igpdi, ipc, inpc
#'
#' \code{ipca}, \code{igpm},\code{igpdi},\code{ipc},\code{incc}, and \code{inpc}.
#'Foi utilizado o INCC-DI, para o reajuste pelo INCC.
#'
#' @references Os índeces podem ser acessados através do acesso ao sítio do IPEA Data
#' \url{http://www.ipeadata.gov.br/}.
#'
#' @return A \code{tibble}, com os valor corrigido, e os índices.
#'
#' @examples
#' # Exemplo, corrigindo pelo ipca:
#' # correct_inflation(valor = 1000, database = "01/2022", indice = "ipca")
#'
#' @importFrom lubridate %m-%
#' @export

correct_inflation <- function(valor = 1000, database = "01/2001", indice = "ipca"){
correct_inflation <- function(valor = 1000, database = "01/2001", indice = "ipca"){

  library(lubridate)
  ajustar_data <- function(database){

    if(nchar(database) != 7 & substr(database, 3, 3) != "/") stop("'database' tem que estar no formato MM/YYYY.")
    lubridate::dmy(paste0("01/", database))
  }

  database <- ajustar_data(database)
  meses_reajuste <- database %m+% months(seq(from = 11, to = 240, by = 12))|>
    tibble::as_tibble_col("Mes")|>
    dplyr::mutate(MesBase = "Aniversario - Valor para o proximo periodo")

  # Get price indice
  message("\nDownloading os dados do IPEA data API\n...\n")
  tmp <- tempfile()

  if(indice == "ipca"){

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_IPCA12')")
    httr::stop_for_status(dados, task = "Buscando os dados do IPCA da API do IPEA.")

  } else if(indice == "igpm") {

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='IGP12_IGPM12')")
    httr::stop_for_status(dados, task = "Buscando os dados do IGP-m da API do IPEA.")

  } else if(indice == "igpdi") {

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='IGP12_IGPDI12')")
    httr::stop_for_status(dados, task = "Buscando os dados do IGP-DI da API do IPEA.")

  } else if(indice == "ipc") {

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='IGP12_IPC12')")
    httr::stop_for_status(dados, task = "Buscando os dados do IPC da API do IPEA.")

  } else if(indice == "inpc"){

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='PRECOS12_INPC12')")
    httr::stop_for_status(dados, task = "Buscando os dados do INPC da API do IPEA.")

  } else if(indice == "incc"){

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='IGP12_INCCG12')")
    httr::stop_for_status(dados, task = "Buscando os dados do INCC da API do IPEA.")

  }

  # Calculate changes in prices
  indice <- httr::content(dados)[[2]]
  indice <- dplyr::bind_rows(indice)[,2:3]
  indice$VALDATA <- lubridate::as_date(indice$VALDATA)
  valorDatabase <- indice$VALVALOR[indice$VALDATA == database %m+% months(-1)]
  indice$Fator <- indice$VALVALOR/valorDatabase
  indice <- indice|> dplyr::select(Mes := 1, Fator)

  df <- indice |>
    dplyr::left_join(meses_reajuste, by = "Mes")|>
    dplyr::mutate(VariacaoMensal = (Fator/dplyr::lag(Fator)-1),
           Variacao12Meses = (Fator/dplyr::lag(Fator,12)-1),
           Fator = dplyr::lag(Fator),
           valorReajustado = Fator*valor,
           MesBase = dplyr::if_else(Mes == database,"Data base",dplyr::lag(MesBase)))|>
    dplyr::select(Mes,Fator,VariacaoMensal,Variacao12Meses,valorReajustado,MesBase)|>
    dplyr::filter(Mes >= database)

  return(df)

}

