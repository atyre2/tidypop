context("test-iterate_pop.R")
parms <- tibble::tibble(t = 1:10, b = 0.2, d = 0.1)
test_that("inputs checked out", {
  expect_error(iterate_pop(), "must provide initial population")
  expect_error(iterate_pop(N0 = 10), "must provide parameter data_frame")
  expect_error(iterate_pop(N0 = 10, parms = parms), "must provide population function")
})

testmodel <- function(N0, b, d, t){
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

test_that("correct return for testmodel", {
  expect_equal(iterate_pop(N0=c(N=10), parms = parms, popfun = testmodel),
               testout)
})


