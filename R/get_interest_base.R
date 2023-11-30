#' Buscar os dados do cdi ou da selic entre duas datas
#'
#' @description
#' \code{get_interest_base()} busca e trata os dados do cdi ou da selic do IPEADATA.
#'
#' @param dtInicio Data de início do período
#' @param dtFim Data final do período.
#' @param indice Indice de juros a ser utilizado (default é "cdi"). Os índices disponíveis são: cdi e selic
#'
#' \code{selic}, and \code{cdi}.
#'
#' @references Os índeces podem ser acessados através do acesso ao sítio do IPEA Data
#' \url{http://www.ipeadata.gov.br/}.
#'
#' @return A \code{tibble}, com os dados dos índices.
#'
#' @examples
#' # get_interest_base(dtInicio = "01/01/2003",dtFim = Sys.Date(), indice = "selic")
#'
#' @importFrom lubridate %m-%
#' @import bizdays
#' @export
get_interest_base <- function(dtInicio = "01/01/2003",dtFim = Sys.Date(), indice = "cdi"){

  indice <- tolower(indice)

  ajustar_data <- function(database){
    if(nchar(database) != 7 & substr(database, 3, 3) != "/") stop("'inicio' tem que estar no formato DD/MM/YYYY.")
    lubridate::dmy(database)
  }


  dtInicio <- ajustar_data(dtInicio)

  # Get price indice
  message("\nDownloading os dados do IPEA data API\n...\n")
  tmp <- tempfile()

  if(indice == "cdi"){

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='SGS366_CDI366')")
    httr::stop_for_status(dados, task = "Buscando os dados do CDI da API do IPEA.")

  } else if(indice == "selic") {

    dados <- httr::GET("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='GM366_TJOVER366')")
    httr::stop_for_status(dados, task = "Buscando os dados da SELIC da API do IPEA.")

  }

  # Calculate changes in prices
  dados <- httr::content(dados)[[2]]
  dados <- dplyr::bind_rows(dados)[,2:3]
  dados$VALDATA <- lubridate::as_date(dados$VALDATA)

  df <- dados |>
    dplyr::select(Data := 1, IndiceDiario:= 2) |>
    dplyr::filter(Data >=dtInicio & Data <= dtFim) |>
    dplyr::filter(!is.na(IndiceDiario)) |>
    dplyr::mutate(Indice = indice,
           FatorDiario = dplyr::if_else(Indice == "cdi", (1+IndiceDiario/100),
                                        dplyr::if_else(Indice == "selic", (1+IndiceDiario/100)^(1/252), NA_real_))) |>
    dplyr::mutate(FatorAcum = cumprod(FatorDiario),
           IndiceAnualizado = ((FatorDiario)^252-1)*100,
           status = "Realizado")

  ##Projeção final do Mês
  datamax <- max(df$Data)
  cdi_ult <- df |>
    dplyr::filter(datamax == Data) |>
    dplyr::select(-Data) |>
    dplyr::mutate(status = "Projetado")

  dias_uteis <- bizdays::bizseq(from = datamax+1, to = Sys.Date()+20, "Brazil/ANBIMA") |>
    tibble::as_tibble_col("Data") |>
    dplyr::filter(lubridate::month(Data)==lubridate::month(Sys.Date()))

  df <- df |>
    dplyr::bind_rows(dias_uteis |> tidyr::crossing(cdi_ult)) |>
    dplyr::mutate(FatorAcum = cumprod(FatorDiario),
           Mes = lubridate::ceiling_date(Data, "month") - lubridate::days(1)) |>
    dplyr::group_by(Mes) |>
    dplyr::mutate(FatorAcumMes = cumprod(FatorDiario))

  return(df)

}
