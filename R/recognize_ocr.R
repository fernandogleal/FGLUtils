#' Recognize OCR from a PDF file and save it to a text file
#'
#' This function takes a PDF file and converts it to text using OCR. The resulting text is then saved to a text file with the same name as the PDF file.
#'
#' @param filepath A character string representing the file path of the PDF file to be converted to text. Defaults to
#' "C:/Users/FLeal/OneDrive - COMPANHIA AIX DE PARTICIPACOES/Área de Trabalho/suzy/atas/2023_03_09.pdf".
#' @return This function doesn't return any value. It saves the recognized OCR from the PDF file to a text file with the same name.
#' @examples
#' # Exemplo, corrigindo pelo ipca:
#' # arquivos <- list.files("C:/Users/FLeal/Área de Trabalho/suzy/atas/", full.names = TRUE) %>%
#' # as_tibble_col("filepath")
#' # pmap(arquivos,reconhecer_ocr)
#'
#' @export

recognize_ocr <- function(filepath = "C:/Users/FLeal/Area de Trabalho/suzy/atas/2023_03_09.pdf"){

  # Set the file path of the PDF file
  pdf_file <- paste0(stringr::str_sub(filepath, end = -4),"pdf")

  # Convert the PDF file to text using OCR
  pdf_text <- tesseract::ocr(pdf_file)

  # Save the text to a TXT file
  txt_file <- paste0(stringr::str_sub(filepath, end = -4),"txt")

  writeLines(pdf_text, txt_file)

  return(pdf_text)

}
