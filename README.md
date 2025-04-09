
<!-- README.md is generated from README.Rmd. Please edit that file -->

# move2Acc

<!-- badges: start -->

[![R-CMD-check](https://github.com/bart1/move2Acc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bart1/move2Acc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bart1/move2Acc/graph/badge.svg)](https://app.codecov.io/gh/bart1/move2Acc)
[![CRAN
status](https://www.r-pkg.org/badges/version/move2Acc)](https://CRAN.R-project.org/package=move2Acc)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Design

- Keep frequency and axes per element or for the vector? -\> per element
  as it changes within study
- Allow NA’s? -\> yes in vector , not in burst?
- Are start time part of vector? -\> probably not as move2 already keeps
  it
- Calibrated (in G) or Uncalibrated -\> might be covered by storing
  units
- We assume constant sampling freq in a burst

## Installation

You can install the development version of move2Acc like so:

``` r
# install.packages("remotes")
remotes::install_github("bart1/move2Acc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(move2Acc)
## basic example code
```
