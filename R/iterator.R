iterate_pop <- function(popfun = NULL, parms = NULL){
  if (is.null(popfun)) stop("must provide population function")
  if (is.null(parms)) stop("must provide parameter dataframe")
  N <- vector("numeric", length = parms$t)
  N[1] <- parms$N0[1]  # first entry in vector is initial population size
  last_t <- length(N)
  # these next lines will cause warnings if
  # b is not either length 1 or (t+1)
  if (length(parms$b) != last_t) parms$b <- rep(parms$b, times=(last_t))
  if (length(parms$d) != last_t) parms$d <- rep(parms$d, times=(last_t))
  # Now we "loop" and calculate N for each time
  # notice that the code is *exactly* like the equation
  for (i in seq_along(parms$t[-last_t])){
    N[i+1] <- popfun(N[i], parms)
  }
  return(data.frame(t=parms$t, b = parms$b, d = parms$d, N = N))
}

exppop <- function(N0, parms){
  N1 <- N0 * (1 + parms$b - parms$d)

  return(N1)
}
