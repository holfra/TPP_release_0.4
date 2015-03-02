#' @title Sigmoidal function for dose response curve fitting
#' @description Return sigmoidal function for curve fit in TPP-CCR experiments
#' @details   the complete function has the following form: 
#' y(bottom, top, infl, hill) <- bottom + (top - bottom) / (1 + exp((infl - x) * hill))
#'  Here, we set top=1 and bottom=0, which leaves two parameters for fitting:
#' y(infl, hill) <- 0 + (1 - 0) / (1 + exp((infl - x) * hill))
#' @return Character of the function.
fctSigmoidCCR <- function(){
  fctStr <- "1 / (1 + exp((infl - x) * hill))"
  return(fctStr)
}
