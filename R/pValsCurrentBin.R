#' @title Determine meltpoints outside of 0.16 and 0.86 percentiles for each bin
#' @references Cox, J., & Mann, M. (2008). MaxQuant enables high peptide 
#' identification rates, individualized ppb-range mass accuracies and 
#' proteome-wide protein quantification. Nature biotechnology, 26(12), 1367-1372.
#' @param mpDiffs numeric vector of melting point differences.
pValsCurrentBin <- function(mpDiffs){
  mpPerc <- quantile(mpDiffs, probs = c(0.1587, 0.5, 0.8413),na.rm=T)
  r_1 = mpPerc[1]
  r0 = mpPerc[2]
  r1 = mpPerc[3]
  pVals = apply(as.matrix(mpDiffs), MARGIN=1, FUN=mpSinglePval, r_1=r_1, r0=r0, r1=r1)
  return(pVals)
}