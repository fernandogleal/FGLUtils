#' Search for files in a specified path recursively
#'
#' @param pattern A string representation of the pattern you want to match.
#' @param root_path A string representation of the root path where the function should start looking for the files. Default is "C:/"
#' @return a tibble with one column named "local_arquivo" that contains the file paths that match the pattern
#' @export
find_files <- function(pattern, root_path = "C:/") {
  regex <- paste0(".*(", pattern, ").*", sep = "")

  file_paths <-
    list.files(
      path = root_path,
      pattern = regex,
      ignore.case = TRUE,
      full.names = TRUE,
      recursive = TRUE
    ) |>
    tibble::as_tibble_col("local_arquivo")

  print(file_paths)

  return(file_paths)
}

