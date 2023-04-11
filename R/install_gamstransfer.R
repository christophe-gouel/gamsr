#' Install gamstransfer package
#'
#' @param type character, indicating the type of package to install. "binary" is
#' the default.
#' @param ... other inputs passed to install.pacakges.
#' @return Invisible 'NULL'.
#' @export
install_gamstransfer <- function(type = "binary", ...) {
  gams_path <- where_is_gams()
  gamstransfer_path <- file.path(gams_path, "apifiles", "R", "gamstransfer")

  sysname <- Sys.info()[["sysname"]]
  if (type == "binary" && sysname != "Linux") {
    if (sysname == "Windows") {
      utils::install.packages(file.path(gamstransfer_path, "binary",
                                        "gamstransfer.zip"),
                              type = "binary",
                              ...)
    } else if (sysname == "Darwin") {
      utils::install.packages(file.path(gamstransfer_path, "binary",
                                        "gamstransfer.tgz"),
                              type = "binary",
                              ...)
    }
  } else {
    utils::install.packages(file.path(gamstransfer_path, "source",
                                      "gamstransfer_r.tar.gz"),
                            dependencies = TRUE,
                            ...)
  }
  return()
}

#' Find the location of GAMS
#'
#' Three strategies are tried in the following order:
#' 1. Find GAMS in the path.
#' 2. Find GAMS from the environment variable GAMSDIR.
#' 3. Find GAMS in the standard installation folder.
#' @return A 'character'.
#' @export
where_is_gams <- function() {
  sysname <- Sys.info()[["sysname"]]

  find_in_default_path <- function(sysname) {
    default_path <- c(Windows = "C:/GAMS",
                      Linux = "/opt/gams",
                      Darwin = "/Applications/GAMS")
    default_path <- default_path[sysname]

    gams_path <- list.files(default_path)
    if (sysname == "Windows") gams_path <- as.integer(gams_path)
    gams_path <- sort(gams_path, decreasing = TRUE)[1]
    gams_path <- ifelse(is.na(gams_path), "",
                        file.path(default_path, gams_path))

    return(gams_path)
  }

  gams_path1 <-
    switch(sysname,
           Windows = system("where.exe gams.exe", intern = TRUE),
           system("which gams", intern = TRUE))
  gams_path1 <- dirname(gams_path1)
  gams_path2 <- sub(";.*$", "", Sys.getenv("GAMSDIR"))
  gams_path3 <- find_in_default_path(sysname)
  if (length(gams_path1 != 0)) {
    return(gams_path1)
  } else if (length(gams_path2) != 0) {
    return(gams_path2)
  } else if (length(gams_path3) != 0) {
    return(gams_path3)
  } else {
    stop("Unable to find GAMS")
  }
}
