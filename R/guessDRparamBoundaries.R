#' @title Compute concentration bounds for sigmoidal curve fitting
#' @description Compute concentration bounds using largest dilution step among the concentration gradient.
#' @param concentrations Drug concentrations applied to aliquots in TPP-CCR experiment.
guessDRparamBoundaries <- function(concentrations){
  conc_diffs   <- diff(concentrations[-1])
  dil_step_size <- max(conc_diffs)
  conc_bds <- c(concentrations[2]-0.5*dil_step_size, concentrations[length(concentrations)]-0.5*dil_step_size)
  return(conc_bds)
}