#' Correção de um valor pelo cdi ou pela selic
#'
#' @description
#' \code{corrigir_juros()} usa dados do IPEADATA para calcular o valor atualizado.
#'
#' @param valor Valor a ser reajustado (default é 1000.00).
#' @param dtInicio Data de início da atualização.
#' @param dtFim Data final da atualização.
#' @param indice Indice de inflação a ser utilizado (default é "igpm"). Os índices disponíveis sã: ipca, igpm, igpdi, ipc, inpc
#'
#' \code{ipca}, \code{selic}, and \code{cdi}.
#'
#' @references Os índeces podem ser acessados através do acesso ao sítio do IPEA Data
#' \url{http://www.ipeadata.gov.br/}.
#'
#' @return A \code{tibble}, com os valor corrigido, e os índices.
#'
#' @examples
#' # Exemplo, corrigindo pela selic:
#' # df <- correct_interest(valor = 1000, dtInicio = "01/01/2003",dtFim = Sys.Date(), indice = "selic")
#'
#' @importFrom lubridate %m-%
#' @export

correct_interest <- function(valor = 1000, dtInicio = "01/01/2003",dtFim = Sys.Date(), indice = "cdi"){

  indice <- tolower(indice)

  ajustar_data <- function(database){
    if(nchar(database) != 7 & substr(database, 3, 3) != "/") stop("'inicio' tem que estar no formato DD/MM/YYYY.")
    lubridate::dmy(database)
  }

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
  dados <- dados |>
    select(Data := 1, IndiceDiario:= 2) |>
    filter(!is.na(IndiceDiario)) |>
    mutate(Indice = indice,
           FatorDiario = if_else(Indice == "cdi", (1+IndiceDiario/100),
                                  if_else(Indice == "selic", (1+IndiceDiario/100)^(1/252), NA_real_))) |>
    select(-IndiceDiario)

  # Parâmetros --------------------------------------------------------------
  ## Data inicial
  dtInicio <- ajustar_data(dtInicio)

  # Encontrar o índice da data imediatamente anterior, caso não exista o dia procurado da data inicial
  dtInicio <- which.max(dados$Data[dados$Data <= dtInicio])

  # Get the closest date
  dtInicio <- dados$Data[dtInicio]

  if(!is.Date(dtFim)){
    dtFim <- ajustar_data(dtFim)
  }else{
    dtFim <- dtFim
  }

  # Encontrar o índice da data imediatamente anterior, caso não exista o dia procurado da data final
  dtFim <- which.max(dados$Data[dados$Data <= dtFim])

  # Get the closest date
  dtFim <- dados$Data[dtFim]

# CDI no período ----------------------------------------------------------
  df <- dados |>
    filter(Data >=dtInicio & Data <= dtFim) |>
    mutate(FatorAcum = cumprod(FatorDiario),
           IndiceAnualizado = ((FatorDiario)^252-1)*100,
           ValorAtualizado = valor*FatorAcum)

  return(df)

}
