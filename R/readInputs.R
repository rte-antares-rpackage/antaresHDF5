#' Read binding constraints
#'
#' @description
#' This function reads the binding constraints of an Antares project \link[antaresRead]{readBindingConstraints}.
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @export
h5ReadBindingConstraints <- function(opts){
  fid <- H5Fopen(opts$h5path)
  timestep <- .getTimStep(fid)[1]
  out <- unserialize(charToRaw(h5read(fid, paste0(timestep , "/inputs/buildingcte"))))
  H5close()
  out
}

#' Import clusters description
#'
#' @description
#' This function reads in the input files of an antares study the
#' characteristics of each cluster \link[antaresRead]{readBindingConstraints}.
#'
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @export
h5ReadLayout <- function(opts){
  fid <- H5Fopen(opts$h5path)
  timestep <- .getTimStep(fid)[1]
  out <- unserialize(charToRaw(h5read(fid, paste0(timestep , "/inputs/layout"))))
  H5close()
  out
}

#' Read areas layout
#'
#' @description
#' This function reads in the input files of an antares study the current areas
#' layout, ie. the position of the areas It may be useful for plotting the
#' network.
#'
#' Be aware that the layout is read in the input files so they may have
#' changed since a simulation has been run \link[antaresRead]{readBindingConstraints}.
#' @param opts \link[antaresRead]{readBindingConstraints}
#'
#' @export
h5ReadClusterDesc <- function(opts){
  fid <- H5Fopen(opts$h5path)
  timestep <- .getTimStep(fid)[1]
  out <- unserialize(charToRaw(h5read(fid, paste0(timestep , "/inputs/cldesc"))))
  H5close()
  out
}
