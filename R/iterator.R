#' Iterate a population model
#'
#' Iterate a population model forward in time from an initial population vector.
#' Parameters of the model can vary with time.
#'
#' @param parms a data frame containing the parameters of the model. Must have
#' one row for each time step.
#' @param N0 a numeric vector of the initial population size. If named, these names
#' will be used in the returned tibble.
#' @param popfun a function that steps the model by one time step.
#' Must take at least N0 as an argument.
#'
#' @details
#' Will fail if any of the three input arguments is NULL. If `parms` only has
#' a single row, just column binds the initial population size. If `parms` has
#' zero rows, the result also has zero rows.
#' @return A data frame containing the input parameters and a column of projected population size.
#' The return value will have the same class as parms.
#' @export
#'
#' @examples
#' # a simple exponential growth model
#' anexppop <- function(N0, b, d) {
#'   N1 <- N0 * (1 + b - d)
#'   return(N1)
#' }
#' # make the input dataframe
#' inputs <- data.frame(Year = 2018:2025, b = 0.2, d = 0.15)
#' iterate(inputs, 23, anexppop)
#' # time dependent model
#' inputs <- data.frame(Year = 2018:2025, b = seq(0.2, 0.1, length = 8), d = 0.15)
#' iterate(inputs, 23, anexppop)
iterate <- function(...) UseMethod("iterate")

#' @export
#' @rdname iterate
iterate.tbl_df <- function(parms = NULL, N0 = NULL, popfun = NULL) {
  if (is.null(N0)) stop("must provide initial population")
  if (is.null(parms)) stop("must provide parameter parms")
  if (is.null(popfun)) stop("must provide population function")
  last_t <- nrow(parms)
  Ndim <- length(N0)
  popfunargs <- names(formals(popfun))
  popfunargs <- popfunargs[popfunargs != "N0"] # remove N0
  # by stashing N in a matrix we enable structured pop'n models
  # I think.
  N <- matrix(0., nrow = last_t, ncol = Ndim)
  if (last_t > 0) {
    # if parms empty, simply skip and return empty dataframe
    N[1, ] <- N0
    # Now we "loop" and calculate N for each time
    # pass N[,] as a matrix to accomodate
    if (last_t > 1) {
      for (i in 1:(last_t - 1)) {
        N[i + 1, ] <- do.call(popfun, c(N0 = list(N[i, ]), as.list(parms[i, popfunargs])))
        if (any(N[i + 1, ] < 0)) {
          warning("Some N at ", i + 1, " were less than 0. Truncating to zero.")
          set2zero <- N[i + 1, ] < 0
          N[i + 1, set2zero] <- 0
        }
      }
    }
  } # last_t > 0


  if (!is.null(names(N0))) {
    # fix column names here -- if done before do.call() c() concatenates colnames
    # in do.call()
    colnames(N) <- names(N0)
  } else {
    # no names, so default to N, N1, etc
    if (Ndim == 1) {
      colnames(N) <- "N"
    } else {
      colnames(N) <- paste0("N", 1:Ndim)
    }
  } # end check names

  return(dplyr::bind_cols(parms, tibble::as_tibble(N)))
}

#' @export
#' @rdname iterate
iterate.data.frame <- function(parms = NULL, N0 = NULL, popfun = NULL) {
  parms <- tibble::as_tibble(parms)
  result <- iterate(parms, N0, popfun)
  return(as.data.frame(result))
}


#' @export
#' @rdname iterate
iterate.NULL <- function(...) {
  stop("iterate() requires input parameters. See ?iterate for details.")
}

#' @export
#' @rdname iterate
iterate.default <- function(...) {
  stop("default method for iterate() not yet implemented.")
}
