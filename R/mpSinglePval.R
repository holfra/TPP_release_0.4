#' @title Compute p-value for melting point difference value
#' @description Compute p-value for a given melting point difference value using 
#' precomputed distribution quantiles.
#' @references Cox, J., & Mann, M. (2008). MaxQuant enables high peptide 
#' identification rates, individualized ppb-range mass accuracies and 
#' proteome-wide protein quantification. Nature biotechnology, 26(12), 1367-1372.
#' @param x ...
#' @param r_1 ...
#' @param r0 ...
#' @param r1 ...
#' @importFrom VGAM erfc
mpSinglePval = function(x, r_1, r0, r1){
  if(!is.na(x)){
    if(x > r0){
      z = (x - r0) / (r1-r0)
      p <- 1/2 * erfc(z/sqrt(2))
    } else {
      z = (r0 - x) / (r0-r_1)
      p <- 1/2 * erfc(z/sqrt(2))
    }
  } else {
    p <- NA_real_
  }
  return(p)
}
