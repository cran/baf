
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baf <img src="man/figures/logo.png" align="right" height="138" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/baf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christopherkenny/baf/actions/workflows/R-CMD-check.yaml)
[![baf status
badge](https://christopherkenny.r-universe.dev/badges/baf)](https://christopherkenny.r-universe.dev/baf)
<!-- badges: end -->

The goal of `baf` is to provide lightweight block assignment (and
equivalence) files from the US Census Bureau.

## Installation

You can install the development version of baf like so:

``` r
pak::pak('christopherkenny/baf')
```

## Example

The workhorse function for `baf` is the `baf()` function.

``` r
library(baf)

baf(state = 'NM', year = 2024, geographies = 'cd')
#> $CD118
#> # A tibble: 107,215 × 2
#>    GEOID           CDFP 
#>    <chr>           <chr>
#>  1 350010001071000 01   
#>  2 350010001071001 01   
#>  3 350010001071002 01   
#>  4 350010001071003 01   
#>  5 350010001071004 01   
#>  6 350010001071005 01   
#>  7 350010001071006 01   
#>  8 350010001071007 01   
#>  9 350010001071008 01   
#> 10 350010001071009 01   
#> # ℹ 107,205 more rows
```

## Cache

By default, `baf` loads files to a temporary directory. To retain files
across sessions, set the `cache_to` argument to a directory of your
choosing. You can also set `options(baf.use_cache = TRUE)` to create a
user-level cache that can persist across projects.
