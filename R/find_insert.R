#' @title Localize and insert values into a data frame
#'
#' @description
#' This function is used to localize a value in a column of a data frame and insert the value of another column from a reference data frame.
#'
#' @param base_classificar A data frame that will receive the insertion of the values.
#' @param nome_coluna_procurar The name of the column in the base_classificar data frame that will be searched for a value.
#' @param base_identificacao A reference data frame containing the values to be inserted.
#' @param nome_coluna_inserir The name of the column in the base_classificar data frame that will receive the insertion of the values.
#'
#' @return The base_classificar data frame with the inserted values.
#' @export
find_insert <- function (base_classificar, nome_coluna_procurar, base_identificacao, nome_coluna_inserir) {

  base_classificar <- base_classificar |>
    dplyr::mutate(inserido := NA) |>
    dplyr::rename(procurar := {
      {
        nome_coluna_procurar
      }
    })

  for (i in 1:nrow(base_identificacao)) {
    base_classificar[grepl(tolower(base_identificacao$procurar[i]),
                           tolower(base_classificar$procurar),
                           fixed = TRUE),]$inserido <-
      base_identificacao$inserir[i]
  }

  base_classificar <- base_classificar |>
    dplyr::rename({
      {
        nome_coluna_inserir
      }
    } := inserido, {
      {
        nome_coluna_procurar
      }
    } := procurar)

  return(base_classificar)
}

