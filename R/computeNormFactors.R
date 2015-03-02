#' @title Compute normalization coefficients for TTP-TR experiments
#' 
#' @description Fit sigmoids to median fold changes of each treatment group and 
#' determine best fit.
#' 
#' @return A list with nomalization curves for each replicate plus additional 
#' information.
#' 
#' @details If the melting curve fitting procedure does not converge, it will be
#' repeatedly started from perturbed starting parameters (maximum iterations 
#' defined by argment \code{maxAttempts}).
#' 
#' @param data List of expressionSets with fold change measurements of normP.
#' @param startPars start values for the melting curve parameters. Will be passed
#' to function \code{\link{nls}} for curve fitting.
#' @param maxAttempts maximal number of curve fitting attempts if model does not converge.
#'
computeNormFactors <- function(data, startPars, maxAttempts){
  message("Computing normalization coefficients:")
  grNames <- names(data)
  
  ## Compute median curves for each treatment group:
  message("1. Computing fold change medians for proteins in normP.")
  yMat <- sapply(grNames, function(n) {esApply(data[[n]], 2, median, na.rm=TRUE)}, simplify=TRUE)
  
  ## Fit sigmoids to each median curve:
  message("2. Fitting melting curves to medians.")
  xMat <- sapply(grNames, function(n) {data[[n]]$temperature}, simplify=TRUE)
  modelList <- sapply(grNames, function(n){fitSigmoidTR(xVec=xMat[,n], yVec=yMat[,n], startPars=startPars, maxAttempts=maxAttempts)}, simplify=FALSE)
  
  ## Melting curve parametes:
  r2 <- sapply(grNames, function(gn) rSquared(model=modelList[[gn]], y=yMat[,gn]))
    
  ## Select best curve fit:
  r2Best <- which.max(r2)
  mBest  <- modelList[[r2Best]]
  message(paste("-> Experiment with best model fit: ", grNames[r2Best], " (R2: ", signif(r2[r2Best],4),")", sep=""))
  
  ## Compute correction factors and store in ExpressionSet object:
  message("3. Computing normalization coefficients")
  y2Mat    <- sapply(grNames, function(n) predict(mBest, newdata=list(x=xMat[,n])))
  dfCoeffs <- y2Mat/yMat
  
  
  ## Return best model:
  return(list("models"      = modelList,
              "medians"     = yMat,
              "tempVals"    = xMat,
              "rSquared"    = r2,
              "bestFit"     = grNames[[r2Best]],
              "corrFactors" = dfCoeffs))
}
