pkg_resource <- function(...) {
  system.file("resources", ..., package = "FGLUtils", mustWork = TRUE)
}
