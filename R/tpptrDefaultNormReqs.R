#' Default filter criteria for fold change normalization
#'
#' Filter criteria as described in the publication.
#'
#' @return List with two entries: 'fcRequirements' describes filtering 
#' requirements on fold change columns, 'otherRequirements' contains criteria on 
#' additional metadata columns.
#'
#' @export
tpptrDefaultNormReqs <- function(){
  dfFcFilters <- data.frame("fcColumn"       = c(7  , 9  , 10),
                            "thresholdLower" = c(0.4, 0  , 0),
                            "thresholdUpper" = c(0.6, 0.3, 0.2),
                            stringsAsFactors = F)
  dfOtherFilters <- data.frame(colName        = "qssm", 
                               thresholdLower = 4, 
                               thresholdUpper = Inf,
                               stringsAsFactors = F) 
  filterCrit <- list("fcRequirements"=dfFcFilters, 
                     "otherRequirements"=dfOtherFilters)
  return(filterCrit)
}