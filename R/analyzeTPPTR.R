#' @title Analyze TPP-TR experiment
#' 
#' @description Performs analysis of a TPP-TR experiment by invoking routines for
#' data import, data processing, normalization, curve fitting, and production of
#' the result table.
#'  
#' @details Invokes the following steps:
#' \enumerate{
#' \item Import data using the \code{\link{tpptrImport}} function.
#' \item Perform normalization (optional) using the \code{\link{tpptrNormalize}} function.
#' To perform normalization, set argument \code{normalize=TRUE}. The normalization
#' will be filtered according to the criteria specified in the \code{normReqs} argument
#' (also see the documentation of \code{\link{tpptrNormalize}} and \code{\link{tpptrDefaultNormReqs}}
#' for further information.
#' \item Fit melting curves using the \code{\link{tpptrCurveFit}} function.
#' \item Produce result table using the \code{\link{tpptrResultTable}} function.
#' \item Export results to Excel using the \code{\link{tppExport}} function.
#' }
#' 
#' The default settings are tailored towards the output of the 
#' python package isobarQuant, but can be customised to your own dataset by the arguments
#' \code{idVar, fcStr, naStrs, qualColName}.
#' 
#' The function \code{analyzeTPPTR} reports intermediate results to the command line. 
#' To suppress this, use \code{\link{suppressMessages}}.
#' 
#' The argument \code{nCores} could be either 'max' (use all available cores) or an 
#' upper limit of CPUs to be used.
#' 
#' The melting curve plots will be stored in a subfolder with name 
#' \code{Melting_Curves} at the location specified by \code{resultPath}.
#' 
#' If the melting curve fitting procedure does not converge, it will be
#' repeatedly started from perturbed starting parameters (maximum iterations 
#' defined by argment \code{maxAttempts}).
#' 
#' @param configTable dataframe, or character object with the path to a 
#'   file, that specifies important details of the TPP-CCR experiment. See 
#'   Section \code{details} for instructions how to create this object.
#' @param data single dataframe, or list of dataframes, containing fold 
#'   change measurements and additional annotation columns to be imported. Can 
#'   be used instead of specifying the file path in the \code{configTable} 
#'   argument.
#' @param resultPath location where to store melting curve plots, intermediate results,
#'  and the final results table.
#' @param idVar character string indicating which data column provides the unique 
#' identifiers for each protein.
#' @param fcStr character string indicating which columns contain the actual 
#'   fold change values. Those column names containing the suffix 
#'   \code{fcStr} will be regarded as containing fold change values.  
#' @param naStrs character vector indicating missing values in the data table. 
#'   When reading data from file, this value will be passed on to the argument 
#'   \code{na.strings} in function \code{read.delim}.
#' @param qualColName character string indicating which column can be used for 
#'   additional quality criteria when deciding between different non-unique 
#'   protein identifiers.
#' @param normalize perform normalization (default: TRUE).
#' @param normReqs list of filtering criteria for construction of the
#' normalization set.
#' @param ggplotTheme ggplot theme for melting curve plots. 
#' @param nCores either a numerical value given the desired number of CPUs, or 'max'
#' to automatically assign the maximum possible number (default).
#' @param startPars start values for the melting curve parameters. Will be passed
#' to function \code{\link{nls}} for curve fitting.
#' @param maxAttempts maximal number of curve fitting attempts if model does not converge.
#' @param binWidth bin width used for p-value computation.
#' @param plotCurves boolan value indicating whether melting curves should 
#' be plotted. Deactivating plotting decreases runtime.
#'
#' @seealso tppDefaultTheme
#' @export
analyzeTPPTR <- function(configTable, data=NULL, resultPath=NULL, 
                         idVar="gene_name", fcStr="rel_fc_", 
                         naStrs=c("NA", "n/d", "NaN", "<NA>"), qualColName="qupm",
                         normalize=TRUE, normReqs=tpptrDefaultNormReqs(),
                         ggplotTheme=tppDefaultTheme(), nCores='max',
                         startPars=c("Pl"=0, "a"=550, "b"=10), maxAttempts=500,
                         binWidth=300, plotCurves=TRUE){
  
  ## Import data:
  trData <- tpptrImport(configTable=configTable, data=data, idVar=idVar, fcStr=fcStr, 
                       naStrs=naStrs, qualColName=qualColName)
  expNames  <- names(trData)
  expCond   <- sapply(trData, function(s) s@annotation[["condition"]])
  expRepl   <- as.numeric(sapply(trData, function(s) s@annotation[["replicate"]]))
  
  ## Extract directory from the filenames in config table, if specified:
  confgTableTmp <- importCheckConfigTable(infoTable=configTable)
  files        <- confgTableTmp$Path
  if (is.null(resultPath)){
    if (!is.null(files)){
      resultPath <- dirname(files[1])
      resultPath <- file.path(resultPath, "TPP_results")
    } else {
      stop("Where would you like the results to be stored? Please specify a folder under argument 'resultPath'.")
    }
  }
  message("Results will be written to ", resultPath)

  ## Create output directory with a subfolder for data objects created during package excecution:
  pathDataObj <- file.path(resultPath, "dataObj")
  if (!file.exists(pathDataObj)) dir.create(pathDataObj, recursive=T)

  ## Save expressionSets with raw data before normalization:
  save(list=c("trData"), file=file.path(pathDataObj, "importedData.RData"))
  
  ## Normalize data:
  if (normalize){
    normResults <- tpptrNormalize(data=trData, normReqs=normReqs)
    trDataNormalized <- normResults[["listAllPNorm"]]
  } else {
    trDataNormalized <- trData
  }
  save(list=c("trDataNormalized"), file=file.path(pathDataObj, "normalizedData.RData"))
  
  ## Fit melting curves:
  trDataFitted <- tpptrCurveFit(data=trDataNormalized, resultPath=resultPath,
                               ggplotTheme=ggplotTheme, doPlot=plotCurves,
                               startPars=startPars, maxAttempts=maxAttempts, nCores=nCores)
  save(list=c("trDataFitted"), file=file.path(pathDataObj, "fittedData.RData"))
  
  ## Analyse melting curves and create result table:
  resultTable <- tpptrResultTable(data=trDataFitted, binWidth=binWidth)
  save(list=c("resultTable"), file=file.path(pathDataObj, "results_TPP_TR.RData"))
  
  ## Save result table in Excel spreadsheet:
  tryCatch(tppExport(tab=resultTable, file=file.path(resultPath, "results_TPP_TR.xlsx")),
           error = function(cond){
             message("\nCaution! Excel spreasheet could not be produced correctly due to the following error:")
             message(cond)
             message("\n\nAlthough the Excel output failed, you can still access the results from the dataframe 'results_TPP_TR.Rdata' in ", pathDataObj, ".\nAlternatively, you can use the return value of function 'analyzeTPPTR'.\n")})
  
  ## --------------------------------------------------------------------------------------------
  ## Create QC plots:
  ## 1. QC plot to illustrate median curve fits:
  pdf(file=file.path(resultPath, "QCplots.pdf"), width=8, height=9)
  message("Creating QC plots to visualize median curve fits...")
  if (normalize){
    qcPlotMedianFit <- normResults[["qcPlotObj"]]
    suppressWarnings(print(qcPlotMedianFit))
  }
  message("done.\n")
  
  ## 2. QC plot to correlate fold changes before and after normalization between all experiments:
  message("Creating QC plots to visualize normalization effects...")
  qcPlotCorrelateGroupsRaw  <- tppQCPlotsCorrelateExperiments(tppData=trData, annotStr="Non-normalized Fold Changes", writePdf=FALSE)
  if (normalize){
    qcPlotCorrelateGroupsNorm <- tppQCPlotsCorrelateExperiments(tppData=trDataNormalized, annotStr="Normalized Fold Changes", writePdf=FALSE)
  }
  for (pn in names(qcPlotCorrelateGroupsRaw)){
    suppressWarnings(print(qcPlotCorrelateGroupsRaw[[pn]]))
    if (normalize) suppressWarnings(print(qcPlotCorrelateGroupsNorm[[pn]]))
  }
  message("done.\n")
  
  ## 3. QC plot to visualize distribution of melting curve parameters:
  if(!is.null(expCond)){
    message("Creating QC plots to visualize minimal slope distributions...")
    suppressWarnings(tpptrQCPlotsMinSlopes_vs_MPdiffs(resultTable=resultTable, 
                                                      expNames=expNames, 
                                                      expRepl=expRepl, 
                                                      expCond=expCond))
    message("done.\n")
  }
  dev.off()
  ## --------------------------------------------------------------------------------------------
  

  return(resultTable)
}
