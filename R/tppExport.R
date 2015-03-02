#' @title Produce Excel table of TPP-TR or TPP-CCR experiment.
#' @param tab Table with results of the TPP analysis.
#' @param file path for storing results table
#' @export
tppExport <- function(tab, file){
  message("Writing results to file: ", file)
  # Boolean columns: Convert TRUE/(FALSE|NA) to YES/NO values
  allCols <- colnames(tab)
  boolCols <- c(grep("model_converged_", allCols, value=T),
                grep("sufficient_data_for_fit_", allCols, value=T),
                grep("passed_filter", allCols, value=T),
                grep("min_pVals_less_0.05_and_max_pVals_less_0.1", allCols, value=T),
                grep("meltP_diffs_have_same_sign", allCols, value=T),
                grep("meltP_diffs_T_vs_V_greater_V1_vs_V2", allCols, value=T),
                grep("minSlopes_less_than_0.06", allCols, value=T),
                grep("fulfills_all_4_requirements", allCols, value=T))
  for (bc in boolCols){
    x <-tab[,bc]
    xNew <- rep(NA_character_, length(x))
    xNew[x==TRUE] <- "Yes"
    xNew[x==FALSE | is.na(x)] <- "No"
    tab[,bc] <- xNew
  }
    
  ## Remove inflection point column from TPP-TR output:
  inflPointColumn <- grep("inflPoint", allCols, value=T)
  tab <- subset(tab, select=!(allCols %in% inflPointColumn))
  allCols <- colnames(tab)
  
  ## Sort proteins in alphabetical order:
  tab <- arrange(df=tab, tab$Protein_ID)
  
  ## Create workbook and fill with table columns:
  wb <- createWorkbook()
  addWorksheet(wb, "Results")
  writeDataTable(wb, sheet="Results",  x=tab, startCol=1, startRow=1, rowNames=FALSE, colNames=TRUE)
    
  ## Add column with links to fitted curve plots:
  linkCol <- grep("Plot", allCols)
  plotLinks <- as.character(tab[,linkCol])
  names(plotLinks) <- ifelse(is.na(plotLinks), "", as.character(tab$Protein_ID))
  plotLinks[is.na(plotLinks)] <- "none"  
  class(plotLinks) <- "hyperlink"
  suppressWarnings(writeData(wb, sheet="Results", x=plotLinks, startCol=linkCol, startRow = 2))
  
  saveWorkbook(wb, file=file, overwrite=T)
  # to do:
  # min_R_sq_controlx_vs_treatmentx	fehlt
  message("File created successfully!\n")
}