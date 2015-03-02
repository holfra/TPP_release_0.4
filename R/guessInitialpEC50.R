#' @title Guess initial pEC50 value
#' @description Guess initial pEC50 value for logistic curve fit of TPP-CCR experiments.
#' @details The estimated value will be used as starting parameter for the curve fitting procedure.
#' @param dose ...
#' @param response ...
#' @param conc_bds ...
guessInitialpEC50 <- function(dose, response, conc_bds) {
  # weight concentration by proximity of corresponding response to 0.5
  w <- 1/abs(response - 0.5)^100
  w[which(w==Inf)] <- 10^300 # set weight to very high value if response = 0.5
  pec50_init <- signif(sum(w * dose)/sum(w),3)
  
  # set pEC50 guess to be within concentration range
  if(pec50_init < conc_bds[1]) {
    pec50_init <- conc_bds[1]
  }
  if(pec50_init > conc_bds[2]) {
    pec50_init <- conc_bds[2]
  }
  
  pec50_init
}