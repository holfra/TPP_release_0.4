#' @title Assign generic experiment names.
#' @param expNames ...
#' @param expectedLength ...
importCheckExperimentNames <- function(expNames, expectedLength){
  isGroups <- FALSE
  if (!is.null(expNames)){
    if (length(expNames)==expectedLength){
      isGroups <- TRUE
    }
  }
  if (isGroups == FALSE){
    expNames <- paste("Experiment_", 1:expectedLength, sep="")
    flagDefaultNames <- TRUE
  } else{
    expNames <- as.character(expNames)
    flagDefaultNames <- FALSE    
  }
  out <- list(expNames        = expNames,
              genericExpNames = flagDefaultNames)
  return(out)
}