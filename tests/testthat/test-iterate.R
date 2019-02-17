context("test-iterate.R")
parms <- tibble::tibble(t = 1:10, b = 0.2, d = 0.1)
test_that("inputs checked out", {
  expect_error(iterate(), "must provide initial population")
  expect_error(iterate(N0 = 10), "must provide parameter data_frame")
  expect_error(iterate(N0 = 10, parms = parms), "must provide population function")
})

testmodel <- function(N0, b, d){
  N1 <- N0 * (1 + b - d)
  return(N1)
}

testout <- tibble::tribble(
  ~t,  ~b,  ~d,          ~N,
  1L, 0.2, 0.1,          10,
  2L, 0.2, 0.1,          11,
  3L, 0.2, 0.1,        12.1,
  4L, 0.2, 0.1,       13.31,
  5L, 0.2, 0.1,      14.641,
  6L, 0.2, 0.1,     16.1051,
  7L, 0.2, 0.1,    17.71561,
  8L, 0.2, 0.1,   19.487171,
  9L, 0.2, 0.1,  21.4358881,
  10L, 0.2, 0.1, 23.57947691
)

# need "as.data.frame" to use base::all_equal otherwise trys joining on
# numbers
test_that("correct return for testmodel", {
  expect_equal(as.data.frame(iterate(parms = parms, N0=10, popfun = testmodel)),
               tolerance = 1e-5,
               as.data.frame(testout))
})

wb_inputs <- tibble::tibble(t = 1961:1966, b = 1.06411639, d = 1.)
testout2 <- tibble::tribble(
  ~t,               ~b, ~d,      ~Population,
  1961L, 1.06411639027921,  1,               10,
  1962L, 1.06411639027921,  1, 10.6411639027921,
  1963L, 1.06411639027921,  1, 11.3234369206085,
  1964L, 1.06411639027921,  1, 12.0494548215122,
  1965L, 1.06411639027921,  1,    12.8220223695,
  1966L, 1.06411639027921,  1, 13.6441241599116
)

test_that("correct return for testmodel 2", {
  expect_equal(as.data.frame(iterate(N0=c(Population=10), parms = wb_inputs, popfun = testmodel)),
               as.data.frame(testout2))
})

inputs3 <- tibble::tibble(t = 1961:1966)
testmodel2 <- function(N0){
  b <- 1.06411639027921
  d <- 1.
  N1 <- N0 * (1 + b - d)
  return(N1)
}

test_that("OK with only N0", {
  expect_equal(as.data.frame(iterate(N0=c(Population=10), parms = inputs3, popfun = testmodel2)),
               as.data.frame(testout2[,c(1,4)]))
})

inputs4 <- tibble::tibble()
test_that("OK with empty tibble",{
  expect_equal(nrow(iterate(N0=c(Population=10), parms = inputs4, popfun = testmodel2)), 0)
}
          )

inputs5 <- tibble::tibble(Year = 1961)
test_that("OK with one row tibble",{
  expect_equal(nrow(iterate(N0=c(Population=10), parms = inputs5, popfun = testmodel2)), 1)
}
)
