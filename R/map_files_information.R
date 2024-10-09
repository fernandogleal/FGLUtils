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
#' file_info <- get_file_info("C:/Users")
#' }
#'
#' @export
map_files_information <- function(directory) {
# directory <- "/mnt/c/Users/FLeal/OneDrive - COMPANHIA AIX DE PARTICIPACOES/Área de Trabalho/programacao/R/projetos/FGL_suzy/05documentos_legais/atas"
    # List all files in the directory recursively
    files <- list.files(directory, recursive = TRUE, full.names = TRUE)

    # Get file information
    file_info <- purrr::map_dfr(files, ~file.info(.x))

    # Add the path as a column and convert to tibble
    file_info <- file_info |>
      tibble::rownames_to_column("path") |>
      tibble::as_tibble()

    # Extract file names
    file_info <- file_info |>
      dplyr::mutate(file = purrr::map_chr(path, basename))

    # Extract directory paths without file names
    file_info <- file_info |>
      dplyr::mutate(dir_path = purrr::map_chr(path, dirname))

    # Create a column with the file extension
    file_info <- file_info |>
      dplyr::mutate(file_extension = tools::file_ext(file))
    
    # Create a column with the file name without extension
    file_info <- file_info |>
      dplyr::mutate(file_name = tools::file_path_sans_ext(file))

    # Split the directory paths into components
    path_components <- purrr::map(file_info$dir_path, ~stringr::str_split(.x, "/")[[1]])

    # Find the maximum number of directory levels
    max_dirs <- max(purrr::map_int(path_components, length))

    # Ensure all paths have the same number of components by padding with NA
    path_components <- purrr::map(path_components, ~{
      length_diff <- max_dirs - length(.x)
      if (length_diff > 0) {
        .x <- c(.x, rep(NA, length_diff))
      }
      .x
    })

    # Create a data frame from the path components
    path_df <- purrr::map_dfr(path_components, ~as.data.frame(t(.x), stringsAsFactors = FALSE))

    # Rename columns to indicate directory levels
    colnames(path_df) <- paste0("dir", stringr::str_pad(1:max_dirs, 2, pad = "0"))

    # Combine the original file info with the new path data frame and file names
    result <- dplyr::bind_cols(file_info, path_df)# |>
      # dplyr::select(-path, -dir_path)  # Remove the original path and dir_path columns

    return(result)
  }
