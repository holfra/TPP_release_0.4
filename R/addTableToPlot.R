#' @title Add table to melting curve plots.
#'
#' @description Adds a table of melting curve parameters to an existing 
#' ggplot object containing the melting curve plots.
#'
#' @details The table is added to an area below the melting curves.
#' 
#' \code{plotObj} is a ggplot object that contains the melting 
#' curves of a protein estimated in different experiments.
#' 
#'  \code{tableDF} contains the curve parameters to be displayed in the 
#'  table. The column containing the names of the experimental groups is
#'  specified by \code{meltVar}.
#' 
#' @seealso \code{\link{plotMeltingCurve}}
#' @family melting curve fitting functions
#' 
#' @param plotObj ggplot object to which table should be added.
#' @param tableDF dataframe containing the table to be added.
#' @param meltVar character specifying the identifier column in tableDF.
#' @param clrs character vector of plot colors for each row in the table. 
#' 
#' @return ggplot object with table added to an area below the melting curves.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit
addTableToPlot = function(plotObj, tableDF, meltVar, clrs){

  naReplacement = -1e6
  tableDF[is.na(tableDF)] = naReplacement
  tableDF_m = melt(tableDF, id=meltVar)
  levels(tableDF_m$condition) <- levels(tableDF$condition)
  tableDF_m = rbind( tableDF_m, data.frame(condition=rep(' ', length(unique(tableDF_m$variable))),
                                           variable=unique(tableDF_m$variable),
                                           value=unique(tableDF_m$variable)))
  tableDF_m = rbind( tableDF_m, data.frame(condition=unique(tableDF_m$condition),
                                           variable=rep(' ', length(unique(tableDF_m$condition))),
                                           value=unique(tableDF_m$condition)))
  tableDF_m[tableDF_m == naReplacement] = '-'
  assign("tableDF_m", tableDF_m, envir=globalenv())

  data_table <- ggplot(tableDF_m, aes(x=factor(variable,
                                               levels=c(' ', (setdiff(unique(tableDF_m$variable) , ' ')))),
                                      y=factor(condition, levels=rev(c(' ', setdiff(levels(tableDF_m$condition), ' ')))),
                                      label = format(value, nsmall = 1),
                                      colour=condition)) +
    geom_text(size = 5, vjust=1.3, fontface='bold') +
    theme_bw() +
    theme(axis.title.x = element_text(size = 11, vjust = 1),  legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(), axis.text.y = element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
    scale_color_manual(values = c(as.character(clrs), 'black'))

  tableHeight = 1.3*(nrow(tableDF)+(2/nrow(tableDF)))
  plotHeight = 22 - tableHeight

  a = arrangeGrob(plotObj, data_table, nrow = 2, heights = unit(c(plotHeight,tableHeight), 'cm' ) )
  # a = arrangeGrob(plotObj, data_table, nrow = 2, heights = unit(c(plotHeight,tableHeight), 'npc' ) )
  # a = arrangeGrob(plotObj, data_table, nrow = 2)
  return(a)
}
