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
    dplyr::mutate(inserido = NA) |>
    dplyr::rename(procurar = dplyr::all_of(nome_coluna_procurar))

  for (i in seq_len(nrow(base_identificacao))) {
    mask <- grepl(tolower(base_identificacao$procurar[i]),
                  tolower(base_classificar$procurar))
    if (any(mask)) {
      base_classificar$inserido[mask] <- base_identificacao$inserir[i]
    }
  }

  base_classificar <- base_classificar |>
    dplyr::rename_with(~ nome_coluna_inserir, .cols = "inserido") |>
    dplyr::rename_with(~ nome_coluna_procurar, .cols = "procurar")

  return(base_classificar)
}

