#' Iterate a population model
#'
#' @param parms a data_frame containing the parameters of the model. Must have
#' arguments t and N0.
#' @param popfun a function that steps the model by one
#'
#' @return a data_frame containing the input parameters and a column of projected population size
#' @export
#'
#' @examples
#'
iterate_pop <- function(parms = NULL, popfun = NULL){
  if (is.null(popfun)) stop("must provide population function")
  if (is.null(parms)) stop("must provide parameter data_frame")
  N <- vector("numeric", length = nrow(parms))
  N[1] <- parms$N0[1]  # first entry in vector is initial population size
  last_t <- length(N)
  # Now we "loop" and calculate N for each time
  for (i in seq_along(parms$t[-last_t])){
    N[i+1] <- popfun(N[i], parms[i,])
  }
  return(tibble::tibble(t=parms$t, b = parms$b, d = parms$d, N = N))
}

#' Exponential population growth
#'
#' @param N0 Initial population size
#' @param parms one row data_frame with model parameters
#'
#' @return a single numeric value which is the next population size
#' @export
#'
exppop <- function(N0, parms){
  N1 <- with(parms,
             N0 * (1 + b[1] - d[1]))
  return(N1)
}
