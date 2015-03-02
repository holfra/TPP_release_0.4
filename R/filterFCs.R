#' @title Filter fold change columns for normalization set construction
#' @description Filter all data sets by specified criteria on fold change columns
#' @details ...
#' @seealso ...
#' @family ...
#' @references ...
#' @param data expressionSet with fold changes for filtering
#' @param pos vector of FC-column positions for filtering
#' @param lb vector of lower bounds
#' @param ub vector of upper bounds
filterFCs <- function(data, pos, lb, ub){
  ## Initialize boolean vector indicating valid rows:
  rValid <- rep(TRUE, nrow(data))

  ## Filter proteins according to lower and upper bounds:
  for (i in 1:length(pos)){
    rValTmp <- rep(TRUE, nrow(data))

    posTmp <- pos[i]
    lbTmp  <- lb[i]
    ubTmp  <- ub[i]

    x      <- exprs(data)[,posTmp]
    lbValid <- x >= lbTmp
    ubValid <- x <= ubTmp

    rValTmp[!lbValid]       <- FALSE
    rValTmp[!ubValid]       <- FALSE
    rValTmp[is.na(lbValid)] <- FALSE
    rValTmp[is.na(ubValid)] <- FALSE
    rValid[rValTmp == FALSE] <- FALSE

    message("  Column ", posTmp, " between ", lbTmp, " and ", ubTmp, " -> ", sum(rValTmp), " out of ", length(rValTmp), " proteins passed")
  }

  fcFiltered = data[rValid,]
  message(sum(rValid), " out of ", length(rValid), " proteins passed in total.\n")
  return(fcFiltered)
}