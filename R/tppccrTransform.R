#' Transform fold changes of TPP-CCR experiment
#' @param data expressionSet object containing the data to be transformed.
#' @param fcCutoff Cutoff for highest compound concentration fold change.
#' @export
tppccrTransform <- function(data, fcCutoff=1.5) {
  message("Transforming data ...")
  fcOrig     <- exprs(data)
  
  ## Mark proteins that are stabilized or destabilized by compound treatment
  fcMaxConc  <- fcOrig[, which.max(data$concentration)]
  flagStab   <- fcMaxConc >= fcCutoff
  flagDestab <- fcMaxConc <= 1/fcCutoff
  compoundEffect                   <- rep(NA_character_, nrow(data))
  compoundEffect[flagStab]         <- "stabilized"
  compoundEffect[flagDestab]       <- "destabilized"
  featureData(data)$CompoundEffect <- compoundEffect
  
  # Iterate through all protein fold change columns and do transformation
  iStab   <- which(flagStab)
  iDestab <- which(flagDestab)
  
  fcNew <- matrix(nrow=nrow(data), ncol=ncol(data), dimnames=list(featureNames(data), colnames(data)))
  for (i in 1:nrow(fcNew)){
    if (i %in% iStab){
      # Transform fc of proteins stabilized by cpd treatment to 
      # fc=0 for DMSO and fc=1 for hightest cpd conc
      fcNew[i,] <- (fcOrig[i,] - 1) / (fcMaxConc[i] - 1)
    } else if (i %in% iDestab){
      # Transform fc of proteins destabilized by cpd treatment to 
      # fc=1 for DMSO and fc=0 for hightest cpd conc
      fcNew[i,] <- (fcOrig[i,]-fcMaxConc[i])/ (1-fcMaxConc[i])
    }
  }
  
  ## Store transformed data in expressionSets and return result
  exprs(data) <- fcNew  
  message("done.")
  return(data)  
}
