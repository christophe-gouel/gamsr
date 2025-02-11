## Write data to GMS

#' Write GMS file
#'
#' This function takes a data frame and writes it to a GMS file. The columns of
#' the data frame, except the last one, are combined into a single column with
#' period (".") as the separator.
#'
#' @param x A data frame with the data in the last column and the sets in all
#'   previous columns.
#' @param file Path to the output file.
#'
#' @return Invisible version of the modified data frame.
#'
#' @examples
#' # Create a data frame
#' df <- data.frame(
#'   i = c(rep("seattle", 3), rep("san-diego", 3)),
#'   j = rep(c("new-york", "chicago", "topeka"), 2),
#'   value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
#' )
#'
#' # Write the data frame to a GMS file
#' write_gms(df, tempfile())
#'
#' @export
write_gms <- function(x, file) {
  x_out <- tidyr::unite(x, col = "sets", 1:(ncol(x) - 1), sep = ".")
  utils::write.table(
    x = x_out,
    file = file,
    sep = "\t",
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE
  )
  return(invisible(x_out))
}
