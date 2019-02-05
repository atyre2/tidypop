#' Iterate a population model
#'
#' @param N0 a numeric vector of the initial population size
#' @param parms a data_frame containing the parameters of the model. Must have
#' arguments t.
#' @param popfun a function that steps the model by one time step. Must take all
#' the columns in parms as arguments, plus N0.
#'
#' @return a data_frame containing the input parameters and a column of projected population size
#' @export
#'
#' @examples
#'
iterate_pop <- function(N0 = NULL, parms = NULL, popfun = NULL){
  if (is.null(N0)) stop("must provide initial population")
  if (is.null(popfun)) stop("must provide population function")
  if (is.null(parms)) stop("must provide parameter data_frame")
  last_t <- nrow(parms)
  Ndim <- length(N0)
  # by stashing this in a matrix we enable structured pop'n models
  # I think.
  N <- matrix(0., nrow = last_t, ncol = Ndim)
  N[1,] <- N0
  # Now we "loop" and calculate N for each time
  # pass N[,] as a matrix to accomodate
  for (i in seq_along(parms$t[-last_t])){
    N[i+1,] <- do.call(popfun, c(N0=N[i,], as.list(parms[i,])))
  }
  return(tibble::tibble(t=parms$t, b = parms$b, d = parms$d, N = N))
}

#' Exponential population growth
#'
#' A simple test function for the package. Deterministic exponential growth
#' with independent parameters for births and deaths. Only projects one time step.
#'
#' @param N0 Initial population size
#' @param b per capita birth rate
#' @param d per capita death rate
#' @param t time index, currently ignored.
#'
#' @return a single numeric value which is the next population size
#' @export
#'
anexppop <- function(N0, b, d, t){
  N1 <- N0 * (1 + b - d)
  return(N1)
}
