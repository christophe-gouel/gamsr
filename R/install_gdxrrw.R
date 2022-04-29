
detect_last_version <- function() {
  gdxrrw_url <- "https://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r"
  gdxrrw_html <- rvest::read_html(gdxrrw_url)
  alist <- rvest::html_elements(gdxrrw_html, "a")
  last_version_url <- rvest::html_attr(
    alist[stringr::str_detect(alist, "tar.gz")][1],
    "href"
  )
  last_version <- stringr::str_split(last_version_url, "gdxrrw_")[[1]][2]
  last_version <- stringr::str_split(last_version, ".tar.gz")[[1]][1]
  return(last_version)
}

select_ext <- function(OS) {
  switch(OS,
    "Windows" = "zip",
    "Linux" = "tar.gz",
    ... = "tgz"
  )
}

#' Install gdxrrw package
#'
#' @param version character string defining the version of the package to
#' install. Default to "latest".
#' @param type character, indicating the type of package to download and
#' install. Possible values are '"platform_default"' (the default) and
#' '"source"'. Except on Windows, the platform default is to install from
#' source. Any other value will be interpreted as installation from binary.
#' @return Invisible 'NULL'.
#' @export
install_gdxrrw <- function(version = "latest", type = "platform_default") {
  OS <- Sys.info()["sysname"]
  if (type == "platform_default") {
    type <- ifelse(OS == "Windows", "binary", "source")
  }
  ext <- ifelse(type == "source", "tar.gz", select_ext(OS))

  if (version == "latest") version <- detect_last_version()

  base_url <- "https://support.gams.com/_media/gdxrrw:gdxrrw_"
  full_url <- paste0(base_url, version, ".", ext)
  gdxrrw_file <- file.path(tempdir(), paste0("gdxrrw_", version, ".", ext))
  download.file(full_url,
    destfile = gdxrrw_file
  )
  install.packages(gdxrrw_file, repos = NULL)
  return(invisible(list()))
}
