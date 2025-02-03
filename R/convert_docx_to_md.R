#' Convert DOCX File to Markdown
#'
#' This function converts a Microsoft Word (DOCX) file to Markdown format while
#' preserving formatting elements such as headers, tables, and images.
#'
#' @param docx_path Character string. Path to the input DOCX file.
#' @param output_path Character string. Path where the output Markdown file will be saved.
#'
#' @return None. The function creates a Markdown file at the specified output path.
#'
#' @details
#' The function uses pandoc with specific options to ensure proper conversion:
#' * Prevents automatic line wrapping
#' * Uses ATX-style headers (#)
#' * Extracts and saves images to a media folder
#' * Preserves tabs
#' * Includes document metadata
#'
#' @examples
#' \dontrun{
#' # Convert a DOCX file to Markdown
#' convert_docx_to_md(
#'   docx_path = "path/to/document.docx",
#'   output_path = "path/to/output.md"
#' )
#' }
#'
#' @export
convert_docx_to_md <- function(docx_path, output_path) {
  # Converter DOCX para markdown com opções extras para preservar formatação
  rmarkdown::pandoc_convert(
    input = docx_path,
    to = "markdown",
    output = output_path,
    options = c(
      "--wrap=none",                    # Evita quebras de linha automáticas
      "--atx-headers",                  # Usa # para cabeçalhos
      "--extract-media=./media",        # Extrai imagens para pasta media
      "--preserve-tabs",                # Mantém tabulações
      "--standalone"                    # Inclui metadados do documento
    )
  )
}
