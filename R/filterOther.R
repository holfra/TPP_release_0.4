#' @title Filter metadata columns for normalization set construction
#' @description Filter all data sets by specified criteria on metadata columns
#' @details ...
#' @seealso ...
#' @family ...
#' @references ...
#' @param data expressionSet with fold changes for filtering
#' @param cols vector of non-FC column names for filtering
#' @param lb vector of lower bounds
#' @param ub vector of upper bounds
filterOther <- function(data, cols, lb, ub){
  ## Initialize boolean vector indicating valid rows:
  rValid <- rep(TRUE, nrow(data))
  
  ## Retrieve potential columns for filtering:
  datCols <- featureData(data)@data
  
  ## Filter proteins according to lower and upper bounds:
  for (i in 1:length(cols)){
    if( cols[i] %in% names(datCols) ){
      rValTmp <- rep(TRUE, nrow(data))
      
      colsTmp <- cols[i]
      lbTmp   <- lb[i]
      ubTmp   <- ub[i]
      
      x       <- datCols[,colsTmp]
      lbValid <- x >= lbTmp
      ubValid <- x <= ubTmp
      
      rValTmp[!lbValid]       <- FALSE
      rValTmp[!ubValid]       <- FALSE
      rValTmp[is.na(lbValid)] <- FALSE
      rValTmp[is.na(ubValid)] <- FALSE
      rValid[rValTmp == FALSE] <- FALSE
      
      message("  Column ", colsTmp, " between ", lbTmp, " and ", ubTmp, "-> ", sum(rValTmp), " out of ", length(rValTmp), " proteins passed")
      
    }else{
      warning(paste( cols[i] , "not found in input columns, can't be used for filtering."))
    }
    
  }
  
  fcFiltered = data[rValid,]
  message(sum(rValid), " out of ", length(rValid), " proteins passed in total.\n")
  return(fcFiltered)
}