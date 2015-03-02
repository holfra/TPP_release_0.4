#' @title Find commonly occurring proteins in each TPP experiment
#' @description Find proteins that commonly occur in all experiments.
#' @details \code{findJointP} is invoked in order to construct normalization set.
#' @return A character vector of protein IDs in jointP.
#' @param data list of expressionSets from which to find joint subset. 
findJointP <- function(data){
  message("Detecting intersect between treatment groups (jointP).")
  grNames <- names(data)

  ## Collect protein IDs detected in each treatment group:
  idList <- sapply(grNames, function(n){featureNames(data[[n]])}, simplify=FALSE)

  ## Determine proteins occurring in each table:
  jointP <- idList[[1]]
  for (n in grNames){
    jointP <- intersect(jointP, idList[[n]])
  }
  message("-> JointP contains ", length(jointP), " proteins.\n")
  return(jointP)
}