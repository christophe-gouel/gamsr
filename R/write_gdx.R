prepare4gdx <- function(x,symName,domains,symText) {
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

#' @export
prepare.set4gdx <- function(x,symName,domains = "*",symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- names(x[1])
  x[[1]] <- as.factor(x[[1]])
  if (ncol(x) > 1) x[[2]] <- as.character(x[[2]])
  if (missing(symText) & ncol(x) > 1) symText <- names(x[2])
  prepare4gdx(x,symName,domains,symText)
}

#' @export
prepare.map4gdx <- function(x,symName,domains,symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- do.call(paste0,c("map",as.list(names(x))))
  for (icol in 1:ncol(x)) x[[icol]] <- as.factor(x[[icol]])
  prepare4gdx(x,symName,domains,symText)
}

#' @export
prepare.par4gdx <- function(x,symName,domains,symText) {
  if (missing(symName)) symName <- deparse(substitute(x))
  if (symName == ".") symName <- names(x[ncol(x)])
  for (icol in 1:(ncol(x)-1)) x[[icol]] <- as.factor(x[[icol]])
  prepare4gdx(x,symName,domains,symText)
}

#' @examples
#' a <- data.frame(i = c("a", "b", "c"), val = 1:3)
#' a <- prepare.par4gdx(a)
#' i <- a[,"i", drop = FALSE]
#' i <- prepare.set4gdx(i)
#' gdxfile <- tempfile(fileext = ".gdx")
#' write_gdx(gdxfile, a, i)
#' @export
write_gdx <- function(gdxName, ...) {
  gdxrrw::wgdx.lst(gdxName, ...)
}
