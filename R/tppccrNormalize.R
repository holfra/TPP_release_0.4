#' @title Normalize data from TPP-CCR experiments
#' @description Normalize each fold change column by its median.
#' @param data expressionSet with measurements to be normalized
#' @export
tppccrNormalize <- function(data){
  message("Normalizing data ...")
  
  ## Compute normalization coefficients:
  fcOld <- exprs(data)
  fcMedians <- apply(fcOld, 2, median, na.rm=TRUE)
  normCoeffs <- 1/fcMedians  
  
  ## Normalize using the computed coefficients:
  dataNew <- applyCoeffs(data=data, coeffs=normCoeffs)
  
  ## Return result
  message("done.")
  return(dataNew)
}