#' Combine PDF files in a folder and save the result
#'
#' This function identifies PDF files in a folder, combines them, and saves the result in a new file.
#'
#' @param Caminho_pasta Character string specifying the path to the folder containing PDF files to be combined.
#' @param pattern Character string specifying the file name pattern to be used to identify PDF files in the folder.
#' @param output_file Character string specifying the name of the output file. Default is "joined.pdf".
#'
#' @return This function does not return a value.
#'
#' @import pdftools
#'
#' @export
#'
merge_pdfs <-
  function (Caminho_pasta,
            pattern = "*.pdf",
            output_file = "joined.pdf") {
    file_list <-
      list.files(Caminho_pasta, pattern = pattern, full.names = TRUE)

    pdftools::pdf_combine(dput(list.files(
      paste0(Caminho_pasta),
      include.dirs = FALSE,
      full.names = TRUE
    )), output = paste0(Caminho_pasta, "/joined.pdf"))

  }


