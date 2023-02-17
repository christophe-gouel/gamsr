# gamsr

Tools for tidy interactions between R and GAMS

## Installation

```r
# install.packages("remotes")
remotes::install_github("christophe-gouel/gamsr");
```

## Usage

`gamsr` is just an opinionated interface to GAMS original package
`gamstransfer` which has to be installed to use `gamsr`. Since `gamstransfer` is not on CRAN
or a public git repo, the following function takes care of the installation.

Install gamstransfer from the local GAMS installation provided that the environment variable GAMSDIR is defined.

```r
install_gamstransfer()
```

Read gdx files

```r
fpath <- system.file("extdata", "trnsport.gdx", package = "gamsr")
read_gdx(fpath, "a") # as tibble
read_gdx(fpath, "a", data_type = "dt") # as data.table
```

Launch GAMS from R

```r
fpath <- file.path(sub(";.*$", "", Sys.getenv("GAMSDIR")), "gamslib_ml", "trnsport.1")
gams(fpath, options = list(output = "NUL", lp = "cplex"))
```

Parallel launch using `furrr` package and read all the gdx files at once

```r
furrr::future_walk(1:10,
                   ~ gams(fpath, options = list(lo = 0, output = "NUL", lp = "cplex"),
				          gdx = paste0(.x, ".gdx")))
read_gdx(paste0(1:10, ".gdx"), "a")
```
