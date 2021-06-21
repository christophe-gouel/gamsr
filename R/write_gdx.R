prepare_4gdx <- function(x,symName,domains,symText) {
  if (any(is.na(x))) warning("Rows with NAs have been removed")
  x <- na.omit(x)
  if (missing(domains)) domains <- rep("*",ncol(x))
  symName <- gsub("[.]","",symName)
  symName <- gsub(" ","",symName)
  attr(x,"domains") <- domains
  attr(x,"symName") <- symName
  if (!missing(symText)) attr(x,"ts") <- symText
  return(x)
}

#' Prepare set for export to gdx
#' 
#' @param x R variable to be prepared for exportation.
#' @param symName Symbol name to be used in gdx file. Use by default the name
#' of the variable to be exported.
#' @param domains optional vector of characters defining the domain over which
#' the parameter is defined. Default to "*".
#' @param symText optional character string defining the explanatory text of
#' the variable. Default to "".
#' @return A 'data.frame' that can be used in the function 'write_gdx'.
#' @export
prepare_set4gdx <- function(x,symName,domains = "*",symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- names(x[1])
  x[[1]] <- as.factor(x[[1]])
  if (ncol(x) > 1) x[[2]] <- as.character(x[[2]])
  if (missing(symText) & ncol(x) > 1) symText <- names(x[2])
  prepare_4gdx(x,symName,domains,symText)
}

#' Prepare mapping for export to gdx
#' 
#' @param x R variable to be prepared for exportation.
#' @param symName Symbol name to be used in gdx file. Use by default the name
#' of the variable to be exported.
#' @param domains optional vector of characters defining the domain over which
#' the parameter is defined. Default to "*".
#' @param symText optional character string defining the explanatory text of
#' the variable. Default to "".
#' @return A 'data.frame' that can be used in the function 'write_gdx'.
#' @export
prepare_map4gdx <- function(x,symName,domains,symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- do.call(paste0,c("map",as.list(names(x))))
  for (icol in 1:ncol(x)) x[[icol]] <- as.factor(x[[icol]])
  prepare_4gdx(x,symName,domains,symText)
}

#' Prepare parameter for export to gdx
#'
#' @param x R variable to be prepared for exportation.
#' @param symName Symbol name to be used in gdx file. Use by default the name
#' of the variable to be exported.
#' @param domains optional vector of characters defining the domain over which
#' the parameter is defined. Default to "*".
#' @param symText optional character string defining the explanatory text of
#' the variable. Default to "".
#' @return A 'data.frame' that can be used in the function 'write_gdx'.
#' @export
prepare_par4gdx <- function(x,symName,domains,symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- names(x[ncol(x)])
  for (icol in 1:(ncol(x)-1)) x[[icol]] <- as.factor(x[[icol]])
  prepare_4gdx(x,symName,domains,symText)
}

#' Write gdx files
#'
#' @param gdxName path to one gdx file
#' @param ... additional arguments to be passed to the function wgdx.lst.
#' @examples
#' a <- data.frame(i = c("a", "b", "c"), val = 1:3)
#' a <- prepare_par4gdx(a)
#' i <- a[,"i", drop = FALSE]
#' i <- prepare_set4gdx(i)
#' gdxfile <- tempfile(fileext = ".gdx")
#' write_gdx(gdxfile, a, i)
#' @export
write_gdx <- function(gdxName, ...) {
  gdxrrw::wgdx.lst(gdxName, ...)
}
