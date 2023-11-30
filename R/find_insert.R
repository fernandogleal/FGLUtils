#' @title Localize and insert values into a data frame
#'
#' @description
#' This function is used to localize a value in a column of a data frame and insert the value of another column from a reference data frame.
#'
#' @param BaseClassificar A data frame that will receive the insertion of the values.
#' @param NomeColunaProcurar The name of the column in the BaseClassificar data frame that will be searched for a value.
#' @param BaseIndentificacao A reference data frame containing the values to be inserted.
#' @param NomeColunaInserir The name of the column in the BaseClassificar data frame that will receive the insertion of the values.
#'
#' @return The BaseClassificar data frame with the inserted values.
#' @export
find_insert <- function (BaseClassificar = extrato2,NomeColunaProcurar = "historico",BaseIndentificacao = base_identificacao,NomeColunaInserir = "pkConta") {

  BaseClassificar <- BaseClassificar |>
    dplyr::mutate(inserido := NA) |>
    dplyr::rename(procurar := {
      {
        NomeColunaProcurar
      }
    })

  for (i in 1:nrow(BaseIndentificacao)) {
    BaseClassificar[grepl(tolower(BaseIndentificacao$procurar[i]),
                          tolower(BaseClassificar$procurar)),]$inserido <-
      BaseIndentificacao$inserir[i]
  }

  BaseClassificar <- BaseClassificar |>
    dplyr::rename({
      {
        NomeColunaInserir
      }
    } := inserido, {
      {
        NomeColunaProcurar
      }
    } := procurar)

  return(BaseClassificar)
}
