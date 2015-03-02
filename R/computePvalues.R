#' @title Compare proteins of vehicle and treatment groups.
#' 
#' @description Compute p-values of melting point differences
#' 
#' @details Proteins are assigned to bins of specified size (default: 300) 
#' according to their ranked minimal slopes. For each bin, p-values are computed 
#' as described by Cox et al. (2008).
#' 
#' @references Cox, J., & Mann, M. (2008). MaxQuant enables high peptide 
#' identification rates, individualized ppb-range mass accuracies and 
#' proteome-wide protein quantification. Nature biotechnology, 26(12), 1367-1372.
#' 
#' @param ids character vector of protein ids.
#' @param minSlopes numeric vector of minimal slopes.
#' @param mpDiffs numeric vector of melting point differences.
#' @param binWidth bin width used for p-value computation.
#' 
computePvalues <- function(ids, minSlopes, mpDiffs, binWidth){
  if (binWidth > length(minSlopes)){
    stop(simpleError(paste("Error in p-value computation: Assigned bin width (",binWidth,") is larger than maximum number of proteins that passed quality control.", sep="")))
  }

  ## Bin melting points according to slope values:
  iValid    <- !is.na(minSlopes)
  minSlopes <- minSlopes[iValid]
  mpDiffs   <- mpDiffs[iValid]
  idsValid  <- ids[iValid]

  mpNum       <- length(minSlopes)
  binWidthRel <- binWidth/mpNum
  binProp <- sort(unique(c(seq(1, 0, by=-binWidthRel), 0)))
  bounds  <- quantile(minSlopes, binProp, na.rm=TRUE)
  bins    <- .bincode(minSlopes, bounds, include.lowest=TRUE, right=TRUE)

  ## If bin with lowest values is smaller than the others, include it into the
  ## succeeding bin:
  if (sum(bins==1) < binWidth) bins[bins==1] <- 2

  ## Compute p-values for each bin
  pVals <- rep(NA_real_, length(bins))
  for (b in unique(bins)){
    iBin <- which(bins==b)
    pVals[iBin] <- pValsCurrentBin(mpDiffs[iBin])
  }

  ## Perform Benjamini-Hochberg correction (over all bins)
  pVals <- p.adjust(pVals, "fdr")

  #plot(abs(minSlope)~ mpDiff, data=subset(mpDiffPvals, pVals<=0.05), col="blue")
  #points(abs(minSlope)~ mpDiff, data=subset(mpDiffPvals, pVals>0.05), col="red")

  ## Output vector
  pOut <- rep(NA_real_, mpNum)
  pOut[iValid] <- pVals
  return(pOut)
}