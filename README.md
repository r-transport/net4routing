
<!-- README.md is generated from README.Rmd. Please edit that file -->

# net4routing

<!-- badges: start -->

<!-- badges: end -->

The goal of net4routing is to prepare route networks, imported from
OpenStreetMap or other sources, for routing with `cppRouting` and other
packages.

## Installation

You can install the development version of net4routing from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("r-transport/net4routing")
```

## Example

``` r
library(net4routing)
## basic example code
```

# Development

To format code for the package, install and format with `air`:

``` sh
curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | sh
air format .
```

Check the package with:

``` r
devtools::check()
```
