# gamsr

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Tools for tidy interactions between R and GAMS

## Installation

```r
# install.packages("remotes")
remotes::install_github("christophe-gouel/gamsr");
```

## Usage

`gamsr` is just an opinionated interface to GAMS original package `gamstransfer`. `gamstransfer` is an R package to read and write GAMS gdx files. It is very complete but also a bit complex when one just wants to quickly read from one or several gdx files. Hence, this package.

Read gdx files

```r
fpath <- system.file("extdata", "trnsport.gdx", package = "gamsr")
read_gdx(fpath, "a") # as tibble
read_gdx(fpath, "a", data_type = "dt") # as data.table
read_gdx(fpath) # Return a data.frame with the list of available symbols
```

Launch GAMS from R

```r
fpath <- file.path(where_is_gams(), "gamslib_ml", "trnsport.1")
gams(fpath, options = list(output = "NUL", lp = "cplex"))
```

Parallel launch using `furrr` package and read all the gdx files at once

```r
# install.packages("furrr")
furrr::future_walk(
  1:10,
  \(num) gams(fpath, options = list(lo = 0, output = "NUL", lp = "cplex"),
              gdx = paste0(num, ".gdx"))
)
read_gdx(paste0(1:10, ".gdx"), "a")
```

Write data to a `gms` file

``` r
df <- data.frame(
  i = c(rep("seattle", 3), rep("san-diego", 3)),
  j = rep(c("new-york", "chicago", "topeka"), 2),
  value = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4)
)
write_gms(df, "distance.gms")
```

## Compatibility with GAMS

This version of `gamsr` uses [GAMS Transfer R](https://www.gams.com/latest/docs/API_R_GAMSTRANSFER.html) as a backend. GAMS Transfer R has been introduced with GAMS 40.1.0. Previous GAMS releases used [GDXRRW](https://github.com/GAMS-dev/gdxrrw) to interact with R. The last version of `gamsr` compatible with GDXRRW can be found in [commit 17470d3](https://github.com/christophe-gouel/gamsr/tree/17470d33edf686c280df5ad9580ed375b9b2731a).

For use with GAMS >= 45, at least release 1.2 should be used because of a change in GAMS Transfer R API.
