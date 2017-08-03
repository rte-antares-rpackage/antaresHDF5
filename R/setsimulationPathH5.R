#' Set simulation path for h5 file
#'
#' @param path \code{character} path of h5 file
#'
#' @export
setSimulationPathH5 <- function(path){
  fid <- H5Fopen(path)
  attributes <- .loadAttributes(fid, "hourly")
  attributes <- attributes$opts
  attributes$h5 <- TRUE
  attributes$h5path <- normalizePath(path)
  options(antares=attributes)
  attributes
}
