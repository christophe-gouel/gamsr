## Read GDX file

#' Read gdx files
#'
#' Read gdx files in a tidy way
#' @param files path to one or several gdx files
#' @param symbol symbol name to read in the gdx. If NULL (default) returns a
#'   data.frame of the list of symbols in the gdx.
#' @param col_names a vector of optional names for the columns.
#' @param attributes character vector: the attributes to keep for variables,
#'   equations and sets. Possible values are '"l"' (the default) specifies the
#'   level, '"m"' specificies the marginal, '"lo"' specificies the lower bound,
#'   '"up"' specifies the upper bound, '"s"' specificies the scale, and '"te"'
#'   specified the text for sets.
#' @param data_type character string: the type of data to output. Possible
#'   values are '"tb"' (the default) for a tibble, '"dt"' for a data.table, and
#'   '"df"' for a data.frame.
#' @param factors_as_strings logical (default is TRUE) specifying whether
#'   factors should be transformed in strings.
#' @param names a vector of optional names in case several gdx files are
#'   imported. The default is to use the gdx file name.
#' @param names_to character string (default: '"name") specifying the new column
#'   to create to store the file names.
#' @return A 'tibble()', a 'data.table', or a 'data.frame'.
#' @examples
#' fpath <- system.file("extdata", "trnsport.gdx", package = "gamsr")
#' read_gdx(fpath, "a")
#' read_gdx(fpath)
#' read_gdx(fpath, "x")
#' read_gdx(fpath, "f")
#' read_gdx(fpath, "i", attributes = "te")
#' @export
read_gdx <- function(files,
                     symbol = NULL,
                     col_names = NULL,
                     attributes = "l",
                     data_type = "tb",
                     factors_as_strings = TRUE,
                     names = NULL,
                     names_to = "name") {

  gdx_cont <- gamstransfer::Container$new(loadFrom = files[1])

  if (!is.null(symbol)) has_symbol <- gdx_cont$hasSymbols(symbol)

  # In the absence of symbol_name returns a data.frame of symbols in the gdx
  if (is.null(symbol) || !has_symbol) {
    dt <- data.frame(type = "",
                     symbol = gdx_cont$listSymbols())
    dt[is.element(dt$symbol, gdx_cont$listVariables()), "type"] <- "variable"
    dt[is.element(dt$symbol, gdx_cont$listEquations()), "type"] <- "equation"
    dt[is.element(dt$symbol, gdx_cont$listSets()), "type"] <- "set"
    dt[is.element(dt$symbol, gdx_cont$listParameters()), "type"] <- "parameter"

    # Change data type
    dt <-
      switch(data_type,
             "tb" = tibble::as_tibble(dt),
             "dt" = data.table::as.data.table(dt),
             "df" = dt,
             dt
             )
    if (is.null(symbol)) {
      return(dt)
    } else {
      message(paste0(
        "The requested symbol ",
        symbol,
        " does not exist in the gdx file.\nHere is the list of available symbols:"
      ))
      print(tibble::as_tibble(dt), n = Inf)
      return(invisible())
    }
  }

  read_gdx_fn <- function(file) {
    read_gdx_single(
      file = file,
      symbol = symbol,
      col_names = col_names,
      attributes = attributes,
      data_type = data_type,
      factors_as_strings = factors_as_strings
    )
  }
  if (is.null(names)) {
    if (length(files) == 1) {
      return(read_gdx_fn(files))
    } else {
      names <- stringr::str_remove(basename(files), ".gdx")
    }
  }

  dt <- lapply(files, read_gdx_fn)
  names(dt) <- names
  dt <- purrr::list_rbind(dt, names_to = names_to)
  return(dt)
}

read_gdx_single <- function(file,
                            symbol,
                            col_names,
                            attributes,
                            data_type,
                            factors_as_strings) {

  # Import data
  gdx_cont <- gamstransfer::Container$new()
  gdx_cont$read(loadFrom = file, symbols = symbol)
  dt <- gdx_cont[symbol]$records

  # Remove unselected attributes
  if (attributes != "all") {
    if (is.element("level", colnames(dt))) {
      variable_attributes <- c(
        l = "level",
        m = "marginal",
        lo = "lower",
        up = "upper",
        s = "scale"
      )
      col_to_remove <- variable_attributes[setdiff(names(variable_attributes),
                                                   attributes)]
      dt[col_to_remove] <- NULL
    } else if (is.element("element_text", colnames(dt)) &&
                 !is.element("te", attributes)) {
      dt["element_text"] <- NULL
    }
  }

  # Rename column
  if (!is.null(col_names)) {
    select_colnames <- 1:min(length(col_names),ncol(dt))  # nolint
    colnames(dt)[select_colnames] <- col_names[select_colnames]
  }

  # Change data type
  dt <-
    switch(data_type,
           "tb" = tibble::as_tibble(dt),
           "dt" = data.table::as.data.table(dt),
           "df" = dt,
           dt
           )

  # Convert factors to strings
  if (factors_as_strings) {
    idx_factor <- (1:ncol(dt))[sapply(dt, is.factor)]
    for (j in idx_factor) dt[, j] <- as.character(dt[, j])
  }

  return(dt)
}
