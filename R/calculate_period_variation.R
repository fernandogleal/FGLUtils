#' Calculate the exchange rate variation between two dates
#'
#' This function receives the exchange rate data and two dates, calculates the exchange rate at the closest date before each of the input dates, and returns a data frame with the period between the two dates, the initial and final dates, the initial and final exchange rate, the number of days in the period, the relative variation in the exchange rate, and the absolute variation.
#'
#' @param basedados A data frame with the exchange rate data, in which the first column is named "Data" and the second column is named "valor".
#' @param DataInicial A character string or a date, representing the start date of the period to be analyzed.
#' @param DataFinal A character string or a date, representing the end date of the period to be analyzed.
#' @return A data frame with the period between the two dates, the initial and final dates, the initial and final exchange rate, the number of days in the period, the relative variation in the exchange rate, and the absolute variation.
#' @examples
#' \dontrun{
#' df <- dados_dolar()
#' datas <-
#' tibble(
#' DataInicial = lubridate::dmy("31/10/2022") %m+% months(seq(from = 1, to = 12, by = 1)),
#' DataFinal = lubridate::dmy("30/11/2022") %m+% months(seq(from = 1, to = 12, by = 1))
#' )
#' tabela <- pmap_dfr(datas,variacao_dolar)
#' }
#'
#' @export
calculate_period_variation <-function(basedados = dolar,DataInicial = "01/01/2022",DataFinal = Sys.Date()) {

  dados <- basedados

    # Parâmetros --------------------------------------------------------------
    ## Data inicial
    if (!is.Date(DataInicial)) {
      DataInicial <- lubridate::dmy(DataInicial)
    } else{
      DataInicial <- DataInicial
    }

    # Encontrar o índice da data imediatamente anterior, caso não exista o dia procurado da data inicial
    DataInicial <- which.max(dados$Data[dados$Data <= DataInicial])

    # Get the closest date
    # dadosInicio <- dados[DataInicial, ] |>
    #   dplyr::rename(DataInicial = Data, ValorInicial = valor) |>
    #   dplyr::mutate(Periodo = "Periodo")

    dadosInicio <- dados[DataInicial, ]
    names(dadosInicio)[names(dadosInicio) == "Data"] <- "DataInicial"
    names(dadosInicio)[names(dadosInicio) == "valor"] <- "ValorInicial"
    dadosInicio$Periodo <- "Periodo"


    ## Data final
    if (!is.Date(DataFinal)) {
      DataFinal <- lubridate::dmy(DataFinal)
    } else{
      DataFinal <- DataFinal
    }

    # Encontrar o índice da data imediatamente anterior, caso não exista o dia procurado da data final
    DataFinal <- which.max(dados$Data[dados$Data <= DataFinal])

    # Get the closest date
    # dadosFim <- dados[DataFinal, ] |>
    #   dplyr::rename(DataFinal = Data, ValorFinal = valor) |>
    #   dplyr::mutate(Periodo = "Periodo")
    dadosFim <- dados[DataFinal, ]
    names(dadosFim)[names(dadosFim) == "Data"] <- "DataFinal"
    names(dadosFim)[names(dadosFim) == "valor"] <- "ValorFinal"
    dadosFim$Periodo <- "Periodo"


    df <- dadosFim |>
      dplyr::left_join(dadosInicio, by = "Periodo") |>
      dplyr::mutate(
        Periodo = paste0(
          "var entre ",
          lubridate::day(DataFinal),
          "/",
          lubridate::month(DataFinal),
          "/",
          lubridate::year(DataFinal),
          " e ",
          lubridate::day(DataInicial),
          "/",
          lubridate::month(DataInicial),
          "/",
          lubridate::year(DataInicial)
        ),
        Intervalo = as.integer(DataFinal - DataInicial),
        VarPer = ValorFinal / ValorInicial - 1,
        VarAbs = ValorFinal - ValorInicial
      ) |>
      dplyr::select(
        Periodo,
        DataInicial,
        ValorInicial,
        DataFinal,
        ValorFinal,
        Intervalo,
        VarPer,
        VarAbs
      )

    return(df)

  }
