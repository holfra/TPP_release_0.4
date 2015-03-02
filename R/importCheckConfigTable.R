#' @title Check whether obligatory experiment information is provided via data 
#' frame or spreadsheet file.
#' @param infoTable dataframe, or character object with the path to a 
#'   file, that specifies important details of the TPP-CCR experiment. See 
#'   Section \code{details} for instructions how to create this object.
#'   @import openxlsx
importCheckConfigTable <- function(infoTable){
  if (is.character(infoTable)){
    if (file.exists(infoTable)){
      ## Determine file extension and import in appropriate format:
      strChunks <- strsplit(infoTable, "\\.")[[1]]
      fileExtension <- strChunks[length(strChunks)]
      if (fileExtension=="txt"|fileExtension=="csv") {
        tab <- read.delim(infoTable, check.names=F) ## Import table from tab-delimited file
      } else if (fileExtension=="xlsx") {
        tab <- read.xlsx(infoTable)  ## Import table in Excel format
      } else {
        stop("Error during data import: the name", infoTable, " does not belong to a valid configuration file.")
      }
    } else {
      stop("Error during data import: the name", infoTable, " does not belong to a valid configuration file.")
    }
    infoTable <- tab
  }
  return(infoTable)
}