#' @title Thermal proteome profiling (TPP)
#'
#' @description \cite{TPP} is a toolbox for analyzing thermal proteome profiling (TPP) 
#' experiments.
#'
#'
#' @docType package
#' @name TPP
#' @param libname a character string giving the library directory where the 
#' package defining the namespace was found. Passed to .onLoad function.
#' @param pkgname a character string giving the name of the package. Passed to .onLoad function.
#' @references 
#' Savitski, M. M., Reinhard, F. B., Franken, H., Werner, T., Savitski, M. F., Eberhard, D., ... & Drewes, G. (2014). Tracking cancer drugs in living cells by thermal profiling of the proteome. Science, 346(6205), 1255784. 
#' @details In order to start a TPP-TR analysis, use function \code{\link{analyzeTPPTR}}. 
#' For a TPP-CCR analysis, use function \code{\link{analyzeTPPCCR}}.
# See the vignette for detailed instructions.
.onLoad <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows") {
    packageStartupMessage("\n ==> If running on Windows, please make sure Rtools is installed correctly and added to your system's path (see vignette).\n")
  }
}
