

#' Constructor function for landscape class objects
#'
#' @param parms list of arguments
#'
#' @return Object of class landscape
#' @export
#'
#' @examples
#' # calling the function without arguments gives a default landscape with 5 patches
#' rl <- createLandscape()
#' plot(rl)
createLandscape <- function(parms = NULL){
  rlandargs <- names(formals(MetaLandSim::rland.graph))
  defaults <- list(mapsize = 1000, dist_m = 30, areaM = 2, areaSD = 0.2, Npatch = 5, disp = 500, plotG = FALSE )
  if(!is.null(parms)){
    provided <- rlandargs %in% names(parms)
    defaults[rlandargs[provided]] <- parms[rlandargs[provided]]
  }
  result <- do.call(MetaLandSim::rland.graph, defaults)
  result
}

#' @rdname createLandscape
#' @export
plot.landscape <- function(rl, ...){
  MetaLandSim::plot_graph(rl, species = FALSE, links = FALSE)
}
