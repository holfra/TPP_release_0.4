#' @title Sigmoidal function for melting curve fitting
#' @description Return sigmoidal function or its derivatives used for curve fit 
#' in TPP-TR experiments
#' @param deriv derivative to return: 0 (default), 1, or 2.
#' @return Function string.
fctSigmoidTR <- function(deriv){
  if (deriv == 0){
    fctStr <- "(1 - Pl) * 1 / (1+exp(-(a/x-b))) + Pl"
  } else if (deriv == 1){
    fctStr <- "-((1 - Pl) * (exp(-(a/x - b)) * (a/x^2))/(1 + exp(-(a/x - b)))^2)"
  } else if (deriv == 2){
    fctStr <- "-((1 - Pl) * 1 * (exp(-(a/x - b)) * (a/x^2) * (a/x^2) - exp(-(a/x - b)) * (a * (2 * x)/(x^2)^2))/
             (1 + exp(-(a/x - b)))^2 - (1 - Pl) * 1 * (exp(-(a/x - b)) * (a/x^2)) *
             (2 * (exp(-(a/x - b)) * (a/x^2) * (1 + exp(-(a/x - b)))))/((1 + exp(-(a/x - b)))^2)^2)"
  }
  return(fctStr)
}