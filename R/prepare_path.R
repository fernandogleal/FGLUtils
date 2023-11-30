#' Prepare Path
#'
#' This function prepares a path for use. It reads from the clipboard or prompts the user for input, removes quotes,
#' replaces backslashes with forward slashes, and then copies the result back to the clipboard.
#'
#' @param path A string specifying the path. If "clipboard", the function will read from the clipboard. Default is "clipboard".
#' @return A string with the prepared path.
#' @export
#'
#' @examples prepare_path()
prepare_path <- function(path = "clipboard") {

  y <- if (path == "clipboard") {

    utils::readClipboard()

  } else {

    cat("Please enter the path:\n\n")

    readline()

  }

  x <- gsub("\"", "", y)
  x <- chartr("\\", "/", x)

  utils::writeClipboard(x)

  return(x)
}

