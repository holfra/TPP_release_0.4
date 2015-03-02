#' @title Normalize protein fold changes obtained by a high-throughput TPP-TR 
#' experiment.
#'
#' @description Normalize fold changes stored in an ExpressionSet object using 
#' a given vector of coefficients.
#'
#' @details This function performs column-wise normalization of the protein fold changes in an expressionSet using a given set of normalization coefficients (also called 'correction factors').
#' Normalization is performed by multiplying each fold change with the corresponding coefficient.
#'
#' @seealso \code{\link{tpptrNormalize}}

#' @family normalization functions

#' @return ExpressionSet containing the matrix of normalized fold changes.
#'
#' @param data ExpressionSet containing matrix of protein fold changes for 
#' normalization. Each row corresponds to a protein and each column to a 
#' temperature condition.
#' @param coeffs Numeric vector or normalization coefficients for each temperature 
#' condition. Length must correspond to number of columns in data.
#'
applyCoeffs <- function(data, coeffs){
  fcOld <- exprs(data)
  fcNorm <- t(apply(X=fcOld, MARGIN=1, function(x) x*coeffs))

  exprs(data)    <- fcNorm
  data$normCoeff <- coeffs

  return(data)
}