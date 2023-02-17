#' Install gamstransfer package
#'
#' @param type character, indicating the type of package to install. "binary" is
#' the default.
#' @param ... other inputs passed to install.pacakges.
#' @return Invisible 'NULL'.
#' @export
install_gamstransfer <- function(type = "binary", ...) {
  gamstransfer_path <-
    file.path(sub(";.*$", "", Sys.getenv("GAMSDIR")), "apifiles", "R",
              "gamstransfer")

  if (type == "binary") {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Windows") {
      install.packages(file.path(gamstransfer_path, "binary",
                                 "gamstransfer.zip"),
                       type = "binary",
                       ...)
    } else if (sysname == "Linux") {
      install.packages(file.path(gamstransfer_path, "binary",
                                 "gamstransfer.tar.gz"),
                       type = "binary",
                       ...)
    } else if (sysname == "Darwin") {
      install.packages(file.path(gamstransfer_path, "binary",
                                 "gamstransfer.tgz"),
                       type = "binary",
                       ...)
    }
  } else {
    install.packages(file.path(gamstransfer_path, "source",
                               "gamstransfer_r.tar.gz"),
                     dependencies = TRUE,
                     ...)
  }
  return()
}
