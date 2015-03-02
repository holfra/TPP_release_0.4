#' @title Plot minSlope vs. melting point differences
#' @description \code{tpptrQCPlotsMinSlopes_vs_MPdiffs} plots the minSlope vs. melting 
#' point difference for each protein and highlights proteins with significant shifts.
#' @export
#' @param resultTable Table of the TPP-TR results.
#' @param expNames Names of each experiment.
#' @param expRepl Replicate each experiment belongs to.
#' @param expCond Condition (Treatment or Vehicle) each experiment belongs to.
tpptrQCPlotsMinSlopes_vs_MPdiffs <- function(resultTable, expNames=NULL, 
                                             expRepl=NULL, expCond=NULL){
  ## Determine whether comparisons should be made between conditions
  ## (currently only possible for the conditions 'Vehicle' and 'Treatment')
  condLevels <- unique(expCond)
  flagCompareMP <- FALSE
  if (length(condLevels)==2){
    if (all.equal(sort(as.character(condLevels)), c("Treatment", "Vehicle"))){
      flagCompareMP <- TRUE
    }
  }
  
  if (flagCompareMP==TRUE){
    for(r in unique(expRepl)){
      expNameV <- expNames[which(expCond=="Vehicle" & expRepl==r)]
      expNameT <- expNames[which(expCond=="Treatment" & expRepl==r)]
      nameMpDiff    <- paste("diff_meltP", expNameT, "vs", expNameV, sep="_")
      nameMinSlope  <- paste("min_slope", expNameT, "vs", expNameV, sep="_")
      namePVal      <- paste("pVal_adj", expNameT, "vs", expNameV, sep="_")
      
      xMpDiff <- resultTable[,nameMpDiff]
      xMinSl  <- resultTable[,nameMinSlope]
      xpVals  <- resultTable[,namePVal]
      
      plotMinSlopes_vs_MPDiffs(mpDiffs=xMpDiff, minSlopes=xMinSl, pValues=xpVals, 
                               expName1=expNameT, expName2=expNameV)
    }    
    
  } else {
    warning("QC plots of melting point differences can only be created when experiments are assigned to the conditions 'Treatment' and 'Vehicle'.")
  }

}