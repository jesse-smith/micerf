
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micerf

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/micerf)](https://CRAN.R-project.org/package=micerf)
[![R-CMD-check](https://github.com/jesse-smith/micerf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jesse-smith/micerf/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jesse-smith/micerf/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jesse-smith/micerf?branch=master)
<!-- badges: end -->

`{micerf}` is a stripped-down version of Multiple Imputation using
Chained Equations (MICE) with Fully Conditional Specification (FCS) of
equations and either random forests or sampling from training values as
models. It is designed for efficiency, stability, and minimal
dependencies. It also provides integration with the `{mlr3}` machine
learning toolkit using suggested packages.

## Installation

You can install the development version of `{micerf}` like so:

``` r
# Install {remotes}
if (!"remotes" %in% installed.packages()) install.packages("remotes")
# Install {micerf} from Github
remotes::install_github("jesse-smith/micerf")
```

Note that you will need to have a Rtools installed on Windows to build
packages from Github.
