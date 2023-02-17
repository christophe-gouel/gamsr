## Launch GAMS

combine.lists <- function(list1, list2) {
  # Combine lists 'list1' and 'list2', giving precedence to elements found in
  # 'list2': that is, if $something is found in both 'list1' and 'list2', the
  # new (output) list will have the same values as 'list2' in $something

  # Version 1.0 (August 2017)
  #
  # Function developed by
  # Patrick Belisle
  # Division of Clinical Epidemiology
  # McGill University Hospital Center
  # Montreal, Qc, Can
  #
  # patrick.belisle@rimuhc.ca
  # http://www.medicine.mcgill.ca/epidemiology/Joseph/PBelisle/BetaParmsFromQuantiles.html


  list1.names <- names(list1)
  list2.names <- names(list2)

  new.list <- list1


  tmp <- match(list2.names, list1.names)
  w <- which(!is.na(tmp))

  if (length(w) > 0) {
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list2[[w]]

    # append elements of 'list2' with unmatched names
    new.list <- c(new.list, list2[-w])
  } else {
    new.list <- c(new.list, list2)
  }

  new.list
} # end of combine.lists


#' Launch GAMS from command line
#'
#' Launch GAMS from command line
#' @param gmsfile path to one gms file
#' @param options list of options
#' @param save path to restart file
#' @param restart path to restart file
#' @param gdx path to gdx file
#' @param envar list of environmental variables
#' @param wd working directory from which to launch the file (default to current
#' working directory)
#' @return A 'tibble()', a 'data.table', or a 'data.frame'.
#' @examples
#' fpath <- file.path(sub(";.*$", "", Sys.getenv("GAMSDIR")),
#'                    "gamslib_ml", "trnsport.1")
#' gams(fpath, options = list(output = "NUL", lp = "cplex"))
#' @export
gams <- function(gmsfile, options = list(), save,
                 restart, gdx, envvar, wd, ...) {
  # Make a GAMS call using all the provided arguments
  default_options <- list(
    lo = 3,
    ll = 0,
    ps = 0,
    pw = 109
  )

  options <- combine.lists(default_options, options)
  options <- purrr::map2(
    options,
    names(options),
    function(opt_value, opt_name) paste0(opt_name, "=", opt_value)
  )
  options <- purrr::reduce(options, paste)

  save <- ifelse(missing(save), "", paste0("save=", save))

  restart <- ifelse(missing(restart), "", paste0("restart=", restart))

  gdx <- ifelse(missing(gdx), "", paste0("gdx=", gdx))

  if (missing(envvar)) {
    envvar <- ""
  } else {
    envvar <- purrr::map2(
      envvar,
      names(envvar),
      ~ paste0("--", .y, "=", .x)
    )
    envvar <- purrr::reduce(envvar, paste)
  }

  gamscl <- paste("gams ", gmsfile, options, save, restart, gdx, envvar)
  if (!missing(wd)) {
    current_wd <- getwd()
    setwd(wd)
  }
  system(gamscl, ...)
  if (!missing(wd)) setwd(current_wd)

}
