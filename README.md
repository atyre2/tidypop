
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidypop

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidypop)](https://cran.r-project.org/package=tidypop)
[![Travis build
status](https://travis-ci.org/atyre2/tidypop.svg?branch=master)](https://travis-ci.org/atyre2/tidypop)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/atyre2/tidypop?branch=master&svg=true)](https://ci.appveyor.com/project/atyre2/tidypop)
[![Codecov test
coverage](https://codecov.io/gh/atyre2/tidypop/branch/master/graph/badge.svg)](https://codecov.io/gh/atyre2/tidypop?branch=master)
<!-- badges: end -->

tidy interface to single species population models

The goal of tidypop is twofold. First, to facilitate teaching simple
single species population models in a tidy context. Second, to provide a
‘tidy’ interface based on data frames to existing population packages.

In a ‘tidy’ context, the main functions should take inputs and produce
outputs that are data frames ready to use with `dplyr` and `ggplot2`.
Using these packages revolutionized the use of R in my classroom.
Unfortunately, I found myself having to push students much farther in
the use of programming than I would like in order to use existing
demographic packages. There was a lot of extra work to convert the
return values into dataframes for plotting purposes. In addition,
population models require iteration, and that also increased the
pressure on their understanding.

I started [thinking about this a couple of years
ago](https://drewtyre.rbind.io/post/teaching-the-logistic-model/) and
this package is the result of that plus many suggestions received on
twitter, especially from @pixxpih. The key idea is to separate the
processing of arguments, iteration of the model, etc, from the one step
transition model. Students can easily write a one-line function
specifying the transition – it is often just a direct translation from a
discrete time equation in their notes.

I also want it to be easy to implement parameters that change with time.
Management changes the parameters of the model, e.g. increasing the per
capita birth rate. The simplest way to do this is to create a dataframe
with one row per time step.

## Installation

tidypop is not available from [CRAN](https://CRAN.R-project.org)

but you can install from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atyre2/tidypop")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidypop)
# a simple exponential growth model
anexppop <- function(N0, b, d){
  N1 <- N0 * (1 + b - d)
  return(N1)
}
# make the input dataframe
inputs <- data.frame(Year = 2018:2025, b = 0.2, d = 0.15)
iterate(inputs, 23, anexppop)
#>   Year   b    d        N
#> 1 2018 0.2 0.15 23.00000
#> 2 2019 0.2 0.15 24.15000
#> 3 2020 0.2 0.15 25.35750
#> 4 2021 0.2 0.15 26.62538
#> 5 2022 0.2 0.15 27.95664
#> 6 2023 0.2 0.15 29.35448
#> 7 2024 0.2 0.15 30.82220
#> 8 2025 0.2 0.15 32.36331
# time dependent model
inputs <- data.frame(Year = 2018:2025, b = seq(0.2, 0.1, length = 8), d = 0.15)
iterate(inputs, 23, anexppop)
#>   Year         b    d        N
#> 1 2018 0.2000000 0.15 23.00000
#> 2 2019 0.1857143 0.15 24.15000
#> 3 2020 0.1714286 0.15 25.01250
#> 4 2021 0.1571429 0.15 25.54848
#> 5 2022 0.1428571 0.15 25.73097
#> 6 2023 0.1285714 0.15 25.54718
#> 7 2024 0.1142857 0.15 24.99974
#> 8 2025 0.1000000 0.15 24.10689
```

## Contributing

Please note that the ‘tidypop’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
