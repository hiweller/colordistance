# colordistance

The aim of `colordistance` is to provide functions for quantifying the differences between colorful objects.

## Installation

`colordistance` is still in development, and you can track it at https://github.com/hiweller/colordistance. 

To install the current version (largely untested) version of `colordistance` in R:

1. Install the [`devtools`](https://github.com/hadley/devtools) package (`install.packages("devtools")`).

2. Install `colordistance` *without* vignettes (long-form documentation) to save time and space or *with* vignettes for offline access to help documents.

  ```{r}
  # Without vignettes
  devtools::install_github("hiweller/colordistance")
  
  # With vignettes
  devtools::install_github("hiweller/colordistance", build_vignettes=TRUE)
  ```
 3. If you chose to install vignettes, you can access help documents by running `help(package="colordistance")` or `vignette("colordistance-introduction")`.

