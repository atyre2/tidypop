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
createLandscape <- function(...){
  rlandargs <- formalArgs(MetaLandSim::rland.graph)
  calledargs <- names(list(...))
  defaults <- list(mapsize = 1000, dist_m = sqrt((1e5*2.4)/pi), areaM = 2, areaSD = 0.2, Npatch = 5, disp = 500, plotG = FALSE )
  if(!is.null(calledargs)){
    provided <- rlandargs %in% calledargs
    defaults[rlandargs[provided]] <- list(...)[rlandargs[provided]]
  }
  result <- do.call(MetaLandSim::rland.graph, defaults)
  result
}

#' @rdname createLandscape
#' @export
plot.landscape <- function(rl, ...){
  MetaLandSim::plot_graph(rl, species = FALSE, links = FALSE)
}
