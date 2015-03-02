#' Select specified featureNames in an ExpressionSet
#' @param exprSet ...
#' @param subset ...
#' Creates a subset of an ExpressionSet that contains only specified featureNames.
exprSubset <- function(exprSet, subset){
  newSet <- exprSet[featureNames(exprSet) %in% subset, ]
  return(newSet)
}