#' @title Check the user-specified number of CPUS.
#' 
#' @description  Assistant function to check the desired number of CPUs for parallel
#' melting curve fitting.
#' 
#' @details If the given number exceeds the available CPUS on the device, the maximum
#' available CPUs will be used instead. If the value 'max' was chosen (the default in 
#' \code{\link{analyzeTPPTR}} and \code{\link{tpptrCurveFit}}), the maximum available
#' CPUS will be used as well.
#' 
#' @return Maximal number of cores detected on the current device.
#' 
#' @param cpus Numeric value with desired maximum number of CPUS, or character 
#' string with value 'max'.
#' @importFrom parallel detectCores
checkCPUs <- function(cpus){
  maxCores <- detectCores()
  if (is.numeric(cpus) & (cpus > maxCores)){
    warning("Selected number of cores (", cpus, ") exceeds those available on your device (", maxCores, ")! Starting parallelization with the ", maxCores, " available cores only.")
    cpus <- "max"
  }
  if (cpus == "max") cpus <- maxCores
  return(cpus)
}
