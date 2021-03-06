# gamsr

Tools for tidy interactions between R and GAMS

## Installation

```r
# install.packages("remotes")
remotes::install_github("christophe-gouel/gamsr");
```

## Usage

`gamsr` is just an opinionated interface to GAMS original package
`gdxrrw` which has to be installed to use `gamsr`. Since `gdxrrw` is not on CRAN
or a public git repo, the following function takes care of the installation.

Install gdxrrw from [GAMS website](https://support.gams.com/gdxrrw:interfacing_gams_and_r)

```r
install_gdxrrw() # Install latest version
install_gdxrrw(version = "1.0.6") # Install a specific version
```

Read gdx files

```r
fpath <- system.file("extdata", "trnsport.gdx", package = "gdxrrw")
read_gdx(fpath, "a") # as tibble
read_gdx(fpath, "a", data_type = "dt") # as data.table
```

Write gdx files

```r
a <- data.frame(i = c("a", "b", "c"), val = 1:3)
a <- prepare_par4gdx(a)
i <- a[,"i", drop = FALSE]
i <- prepare_set4gdx(i)
j <- prepare_set4gdx(1:10, symName = "j")
gdxfile <- tempfile(fileext = ".gdx")
write_gdx(gdxfile, a, i, j)
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
