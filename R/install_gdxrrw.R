#' Install gdxrrw package
#'
#' @param version character string defining the version of the package to
#' install. Default to "latest" for the latest release. Could also be "dev"
#' for the development version or a commit, tag, or branch name from the gdxrrw
#' repo (<https://github.com/GAMS-dev/gdxrrw>).
#' @param ... other inputs passed to install_github.
#' @return Invisible 'NULL'.
#' @export
install_gdxrrw <- function(version = "latest", ...) {
  if (version == "latest") {
    remotes::install_github("GAMS-dev/gdxrrw/gdxrrw",
                            ref = remotes::github_release(),
                            ...)
  } else if (version == "dev") {
    remotes::install_github("GAMS-dev/gdxrrw/gdxrrw",
                            ref = "HEAD",
                            ...)
  } else {
    remotes::install_github("GAMS-dev/gdxrrw/gdxrrw",
                            ref = version,
                            ...)
  }
  return(invisible(list()))
}
