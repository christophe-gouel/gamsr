% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_gms.R
\name{write_gms}
\alias{write_gms}
\title{Write GMS file}
\usage{
write_gms(x, file)
}
\arguments{
\item{x}{A data frame with the data in the last column and the sets in all
previous columns.}

\item{file}{Path to the output file.}
}
\value{
Invisible version of the modified data frame.
}
\description{
This function takes a data frame and writes it to a GMS file. The columns of
the data frame, except the last one, are combined into a single column with
period (".") as the separator.
}
\examples{
# Create a data frame
df <- data.frame(
  i = c(rep("seattle", 3), rep("san-diego", 3)),
  j = rep(c("new-york", "chicago", "topeka"), 2),
  value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
)

# Write the data frame to a GMS file
write_gms(df, tempfile())

}
