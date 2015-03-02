#' Determine R2 of a fitted model
#' @param model ...
#' @param y ...
rSquared <- function(model, y) {
  if (class(model)!="try-error"){
    ssTot <- sum((y - mean(y, na.rm=T))^2, na.rm=T)
    ssRes <- sum( (y - predict(model))^2 , na.rm=T )    
    r2 <- 1 - ssRes/ssTot
  } else {
    r2 <- NA_real_
  }
  return(r2)
  
  #   res <- residuals(model)
  #   ss_res <- sum(res^2)
  #   ss_tot <- sum((y-mean(y))^2)
  #   R2 <- signif(1 - ss_res/ss_tot, 3) # R squared
}