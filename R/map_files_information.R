#' Map Files Information
#'
#' This function lists all files in a directory and its subdirectories, 
#' gets information about each file, converts the row names of the file 
#' information into a column, and returns the resulting tibble.
#'
#' @param directory A character string specifying the path to the directory.
#'
#' @return A tibble with information about each file in the directory and its subdirectories.
#' The tibble includes a column "caminho" which contains the path to each file.
#'
#' @examples
#' \dontrun{
#' map_files_information("your/directory/path")
#' }
#'
#' @export
map_files_information <- function(directory) {
  
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  
  file_info <- purrr::map_dfr(files, ~file.info(.x))
  
  file_info <- file_info |> tibble::rownames_to_column("caminho") |> tibble::as_tibble()
  
  return(file_info)
}
