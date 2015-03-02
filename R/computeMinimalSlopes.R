#' @title Minimal slopes per protein and replicate
#' 
#' @description Determine the minimum of the melting curve slopes estimated for Vehicle and Treatment 
#' group for each protein.
#' 
#' @return Vector numeric vector with minimal slope.
#' 
#' @param slV numeric vector with slope estimates of Vehicle group.
#' @param slT numeric vector with slope estimates of Treatment group.
#' 
computeMinimalSlopes <- function(slV, slT){
  # Conversion will prevent warnings in 'min':
  slV <- mapvalues(slV, NA, Inf, warn_missing=FALSE) 
  slT <- mapvalues(slT, NA, Inf, warn_missing=FALSE) 
  ## Compute minima:
  minSlopes <- apply(cbind(slV, slT), 1, min)
  ## Convert back to prevent Inf values in final output:
  minSlopes <- mapvalues(minSlopes, Inf, NA, warn_missing=FALSE) 
}