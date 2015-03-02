#' @title Assistant function that returns the column names of the melting curve parameters
#' in the internal datasets.
#' @details This function is intended to assure consistency when accessing, manipulating, or 
#' storing melting curve paramter columns in the package's data objects.
#' @param returnParNames ...
#' @param returnPerformanceInfo ...
meltCurveParamNames <- function(returnParNames=T, returnPerformanceInfo=T){
  out <- c()
  if (returnParNames)        out <-c(out, "meltPoint", "inflPoint", "slope", "plateau", "R_sq")
  if (returnPerformanceInfo) out <- c(out, "model_converged", "sufficient_data_for_fit")
  return(out)
}