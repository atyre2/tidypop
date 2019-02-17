#' Iterate a population model
#'
#' Iterate a population model forward in time from an initial population vector.
#' Parameters of the model can vary with time.
#'
#' @param N0 a numeric vector of the initial population size. If named, these names
#' will be used in the returned tibble.
#' @param parms a tibble containing the parameters of the model. Must have
#' one row for each time step.
#' @param popfun a function that steps the model by one time step.
#' Must take at least N0 as an argument.
#'
#' @return a data_frame containing the input parameters and a column of projected population size
#' @export
#'
#' @examples
#'
iterate <- function(parms = NULL, N0 = NULL, popfun = NULL){
  if (is.null(N0)) stop("must provide initial population")
  if (is.null(parms)) stop("must provide parameter data_frame")
  if (is.null(popfun)) stop("must provide population function")
  last_t <- nrow(parms)
  Ndim <- length(N0)
  popfunargs <- names(formals(popfun))
  popfunargs <- popfunargs[popfunargs!="N0"] # remove N0
  # by stashing N in a matrix we enable structured pop'n models
  # I think.
  N <- matrix(0., nrow = last_t, ncol = Ndim)
  if (last_t > 0){
    # if parms empty, simply skip and return empty dataframe
    N[1,] <- N0
    # Now we "loop" and calculate N for each time
    # pass N[,] as a matrix to accomodate
    for (i in seq_along(parms$t[-last_t])){
      N[i+1,] <- do.call(popfun, c(N0=N[i,], as.list(parms[i,popfunargs])))
    }

  } # last_t > 0

  if(!is.null(names(N0))){
    # fix column names here -- if done before do.call() c() concatenates colnames
    # in do.call()
    colnames(N) <- names(N0)
  } else {
    # no names, so default to N, N1, etc
    if (Ndim==1){
      colnames(N) <- "N"
    } else {
      colnames(N) <- paste0("N", 1:Ndim)
    }
  } # end check names

  return(dplyr::bind_cols(parms, tibble::as_tibble(N)))
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
