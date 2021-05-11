## Read GDX file

rgdx.var <- function(gdxName, symName, names = NULL, attr = "l", ...) {
  lt <- gdxrrw::rgdx(gdxName, list(name = symName, field = attr),
                     squeeze = FALSE, ...)
  if (is.null(names) | length(names) != (lt[["dim"]] + 1)) {
    domains <- lt[["domains"]]
    defaultDomainsNames <- c(".i", ".j", ".k", ".l")
    domains[domains == "*"] <- defaultDomainsNames[1:sum(domains == "*")]
    domains[domains == "_field"] <- "attribute"
    names <- c(domains, symName)
  }
  colnames(lt[["val"]]) <- names
  dt <- tibble::as_tibble(lt[["val"]])
  for (set in 1:lt[["dim"]])
    dt[[set]] <- lt[["uels"]][[set]][dt[[set]]]
  if (attr == "all")
    dt[["attribute"]] <- lt[["uels"]][[length(lt[["uels"]])]][dt[["attribute"]]]
  return(dt)
}

read_gdx_single <- function(file, symName, col_names = NULL, attribute = "l",
                            te = TRUE, data_type = "tb", ...) {
  fileInfo <- gdxrrw::gdxInfo(file, dump = FALSE, returnList = TRUE)
  nature <- paste0(
    "parameters"[is.element(symName, fileInfo$parameters)],
    "sets"[is.element(symName, fileInfo$sets)],
    "variables"[is.element(symName, fileInfo$variables)])
  sym <- switch(nature,
         "parameters" = try(gdxrrw::rgdx.param(file, symName, names = col_names, ...), TRUE),
         "sets" = gdxrrw::rgdx.set(file, symName, names = col_names, te = te, ...),
         "variables" = rgdx.var(file, symName, names = col_names, attr = attribute, ...))
  if (is.character(sym)) {
    if (is.null(col_names)) col_names <- symName
    sym <- tibble::tibble(rlang::expr(!!col_names := gdxrrw::rgdx.scalar(file, symName))) 
  } else sym <- tibble::as_tibble(sym)
  sym <- switch(data_type,
                "tb" = sym,
                "dt" = data.table::as.data.table(sym),
                "df" = as.data.frame(sym),
                sym)
  return(dplyr::mutate(sym, across(where(is.factor), as.character)))
}

#' Read gdx files
#'
#' Read gdx files in a tidy way
#' @param files path to one or several gdx files
#' @param symName symbol name to read in the gdx
#' @param col_names a vector of optional names for the columns. The default is
#' to use the names in the gdx if existing.
#' @param names a vector of optional names in case several gdx files are
#' imported. The default is to use the gdx file name.
#' @param attribute character string: the attribute for variables. Possible
#' values are '"l"' (the default) specifies the level, '"m"' specificies the
#' marginal, '"lo"' specificies the lower bound, and '"up"' specifies the upper
#' bound.
#' @param data_type character string: the type of data to output. Possible
#' values are '"tb"' (the default) for a tibble, '"dt"' for a data.table, and
#' '"df"' for a data.frame.
#' @param ... additional arguments to be passed to gdxrrw functions.
#' @return A 'tibble()', a 'data.table', or a 'data.frame'.
#' @examples 
#' fpath <- system.file("extdata", "trnsport.gdx", package="gdxrrw")
#' read_gdx(fpath, "a")
#' read_gdx(fpath, "x")
#' read_gdx(fpath, "i", te = FALSE)
#' @export
read_gdx <- function(files, symName, col_names = NULL, names = NULL,
                     attribute = "l", data_type = "tb", ...) {
  read_gdx_fn <- function(file)
    read_gdx_single(file = file, symName = symName, col_names = col_names,
                    attribute = attribute, data_type = data_type, ...)
  if (is.null(names)) {
    if (length(files) == 1) return(read_gdx_fn(files))
    else names <- stringr::str_remove(basename(files), ".gdx")
  }
  return(purrr::map2_dfr(files, names,
                         function(filename, name)
                           dplyr::mutate(read_gdx_fn(filename), name = name)))
}
