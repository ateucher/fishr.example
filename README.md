
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fishr

<!-- badges: start -->

[![R-CMD-check](https://github.com/ateucher/fishr.example/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ateucher/fishr.example/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of fishr is to provide functions for calculating Catch Per Unit
Effort (CPUE), a fundamental metric in fisheries science.

## Installation

You can install the development version of fishr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yourname/fishr")
```

## Example usage

``` r
library(fishr)
cpue(catch = 100, effort = 10)
```

    ## [1] 10

``` r
# With gear standardization
cpue(catch = 100, effort = 10, gear_factor = 0.5)
```

    ## [1] 5
