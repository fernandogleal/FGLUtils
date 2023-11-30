#' Reinstall one of the FGL packages
#'
#' @param pacote The name of the package to reinstall. It can be "FGLUtils", "FGLReports","FGLDerivs","pkFGLFinPe","pkAIX".
#' Default is "FGLUtils"
#' @return None
#' @export
#' @examples
#' # reinstall_fgl_packages("FGLUtils")
#' # reinstall_fgl_packages("FGLReports")
#' # reinstall_fgl_packages("FGLDerivs")
#' # reinstall_fgl_packages("pkFGLFinPe")
#' # reinstall_fgl_packages("pkAIX")
reinstall_fgl_packages <- function(pacote = "FGLUtils") {

  computer <- Sys.info()["nodename"]

  root_path <- switch(computer,
                      "FLEAL-DELL" = paste0("C:/Users/fleal/Google Drive/R/FGL_package/", pacote),
                      "SURFACE-FG" = paste0("C:/Users/fleal/My Drive/R/FGL_package/", pacote),
                      "DESKTOP-FOTV0GM" = paste0("C:/Users/FLeal/Meu Drive/R/FGL_package/", pacote),
                      "workDesktop" = "C:/Users/workDesktop/Documents",
                      "NB-AIX-001" = paste0("C:/Users/FLeal/GoogleDrive/R/FGL_package/", pacote),
                      "Other"
  )

  if (require(pacote, character.only = TRUE)) {
    detach(paste0("package:",pacote), unload=TRUE, character.only = TRUE)
  }

  message(root_path)
  devtools::document(root_path)
  devtools::build(root_path)

  if (require(pacote, character.only = TRUE)) {
    detach(paste0("package:",pacote), unload=TRUE, character.only = TRUE)
  }

  utils::install.packages(paste0(root_path,"_0.0.0.9000.tar.gz"), repos = NULL, type = "source")
}



