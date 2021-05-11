# gamsr

Tools for tidy interactions between R and GAMS

## Installation

```r
# install.packages("devtools")
devtools::install_github("christophe-gouel/gamsr");
```

## Usage

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
a <- prepare.par4gdx(a)
i <- a[,"i", drop = FALSE]
i <- prepare.set4gdx(i)
gdxfile <- tempfile(fileext = ".gdx")
write_gdx(gdxfile, a, i)
```
