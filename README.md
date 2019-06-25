
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidypop

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidypop)](https://cran.r-project.org/package=tidypop)
[![Travis build
status](https://travis-ci.org/atyre2/tidypop.svg?branch=master)](https://travis-ci.org/atyre2/tidypop)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/atyre2/tidypop?branch=master&svg=true)](https://ci.appveyor.com/project/atyre2/tidypop)
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

but you can install the stable version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atyre2/tidypop")
```

or the development version with

``` r
devtools::install_github("atyre2/tidypop@dev")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidypop)
## basic example code
```
