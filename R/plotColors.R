#' @title Create matching color pairs for vehicle and treatment groups
#' @importFrom RColorBrewer brewer.pal
#' @param expConditions ...
#' @param expReplicates ...
plotColors <- function(expConditions, expReplicates){
  groupColors <- FALSE
  if (all(!is.na(expConditions)) && all(!is.na(expReplicates))){
    expReplicates <- as.numeric(expReplicates)
    condLevels <- sort(unique(expConditions))
    repLevels  <- sort(unique(expReplicates))
    condNum <- length(condLevels)
    repNum  <- length(repLevels)
    
    ## Check if several experiments within a replicate got the same condition assigned
    ## (can happen, for example, when all repliacate entries got default value "1")
    if (max(table(paste(expConditions, expReplicates))) == 1){
      ## Check if numbers of conditions and replicates do not exceed maximum
      if (condNum==2 && repNum<=6){ # brewer pal can only produce up to 6 color pairs with 2 intensities each
        if(all.equal(condLevels, c("Treatment", "Vehicle")) & all.equal(repLevels, 1:repNum)){
          groupColors <- TRUE
        }
      }
    }
  }
  
  if (groupColors==TRUE){
    allPairs <- brewer.pal(12, "Paired")
    lightCols <- allPairs[seq(1,12,2)]
    darkCols  <- allPairs[seq(2,12,2)]
    colorVec <- rep(NA, length(expConditions))
    for (r in repLevels){
      iT <- which(expConditions=="Treatment" & expReplicates==r)
      iV <- which(expConditions=="Vehicle" & expReplicates==r)
      colorVec[iT] <- darkCols[r]
      colorVec[iV] <- lightCols[r]
    }
  } else if (groupColors==FALSE){
    colorVec <- brewer.pal(max(length(expConditions),3), "Set1")[1:length(expConditions)]
  }
  return(colorVec)
}