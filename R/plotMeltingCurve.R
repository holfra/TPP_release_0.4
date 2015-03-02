#' @title Plot melting curve fitted to TPP-TR fold changes
#'
#' @details Produce a ggplot to vizualise fitted melting curves, melting points and slopes.
#'
#' @param modelList list of fitted melting curve models. Field names must 
#' correspond to experiment names.
#' @param xMat matrix of temperature values (row-wise) for each experiment. 
#' Rownames must correspond to experiment names.
#' @param fcMat matrix of fold change values (row-wise) for each experiment. 
#' Rownames must correspond to experiment names.
#' @param curvePars matrix of estimated melting curve parameters (row-wise) for 
#' each experiment. Rownames must correspond to experiment names.
#' @param protID name of current protein. Will appear in title plot.
#' @param filename file name of plot. If NULL, plot will be discarded. 
#' @param plotTheme ggplot theme for current plot
#' @param expConditions experiment conditions (typically
#' "Treatment" or "Vehicle")
#' @param expReplicates experimental replicates (typically 1,2, ...)
#' @param addLegend Add legend to plots. Legend is disabled if OS is not Windows or Linux.
#' @import ggplot2
plotMeltingCurve <- function(modelList, xMat, fcMat, curvePars, protID, filename, 
                             plotTheme, expConditions, expReplicates, addLegend){
  if(all(is.na(fcMat))){
    return(NULL)
  } else {
    grNames <- names(modelList)
    yMax <- 1.5
    theme_set(plotTheme)
    
    ## Prepare data objects for plotting:
    ## 1) Create long data tables for ggplot function:
    ## 1.1) Predicted values:
    xLen      <- 100
    xMatLarge <- sapply(grNames, function(g) seq(from=min(xMat[g,]), to=max(xMat[g,]), length.out=xLen))
    yPred     <- sapply(grNames, function(g) robustNlsPredict(model=modelList[[g]], newdata=list(x=xMatLarge[,g])))
    
    ## Determine order of group factors so that they form alternating
    ## Treatment/Vehicle pairs in the ggplot legend:
    plotCols <- plotColors(expConditions, expReplicates)
    names(plotCols) <- grNames
    grOrder <- c()
    for (r in unique(expReplicates)){
      iT <- which(expConditions=="Treatment" & expReplicates==r)
      iV <- which(expConditions=="Vehicle" & expReplicates==r)
      grOrder <- c(grOrder, grNames[iT], grNames[iV])
    }
    if (length(grOrder) == 0) grOrder <- grNames
    plotCols <- plotCols[grOrder]
    
    groupCol1 <- factor(rep(grNames, each=xLen), levels=grOrder)
    plotData1 <- data.frame(Group       = groupCol1,
                            Temperature = numeric(length(grNames)*xLen),
                            FoldChange  = numeric(length(grNames)*xLen),
                            DataType    = "Model")
    for (g in grNames) plotData1[plotData1$Group==g, c("Temperature","FoldChange")] = cbind(xMatLarge[,g], yPred[,g])
    
    ## 1.2) Measured values:
    groupCol2 <- factor(rep(grNames, each=ncol(fcMat)), levels=grOrder)
    plotData2 <- data.frame(Group       = groupCol2,
                            Temperature = numeric(length(grNames)*ncol(fcMat)),
                            FoldChange  = numeric(length(grNames)*ncol(fcMat)),
                            DataType    = "Measured")
    for (g in grNames) plotData2[plotData2$Group==g, c("Temperature","FoldChange")] = cbind(xMat[g,], fcMat[g,])
    ## Replace NAs by negative values in order to suppress warnings of geom_point() function later:
    #plotData2$FoldChange <- mapvalues(plotData2$FoldChange, from=NA, to=-Inf, warn_missing=F)
    
    ## 1.3) Melting points:
    xMP <- subset(curvePars, select=meltPoint)
    yMP <- sapply(grNames, function(g) robustNlsPredict(modelList[[g]], newdata=list(x=xMP[g,])))
    
    groupCol3 <- factor(names(yMP), levels=grOrder)
    plotData3 <- data.frame(Group=groupCol3, yMP=yMP, xMP=xMP[,"meltPoint"])
    
    ## 2) Data frame with curve parameters:
    tableDF <- data.frame(condition = factor(grNames, levels=grOrder),
                          meltPoint = round(curvePars[grNames,"meltPoint"],2),
                          slope     = signif(curvePars[grNames,"slope"],2),
                          plateau   = round(curvePars[grNames,"plateau"],2),
                          R2        = round(curvePars[grNames,"R_sq"],2))
    ## Create plot:
    p <- ggplot()
    p <- p + scale_color_manual(values = plotCols)
    p <- p + scale_y_continuous(limits = c(0, yMax))
    if (addLegend){
      p <- p + theme(legend.position=c(1, 1), legend.justification=c(1,1), legend.title=element_blank())      
    } else {
      p <- p + theme(legend.position="none")      
    }
    p <- p + ggtitle(protID)
    p <- p + xlab(paste('Temperature [\U00B0', 'C]', sep='')) + ylab("Fraction non-denatured")
    
    p <- p + geom_line(data=plotData1, aes(x=Temperature, y=FoldChange, colour=Group), size=1)
    p <- p + geom_point(data=plotData2, aes(x=Temperature, y=FoldChange, colour=Group), na.rm=T)
    p <- p + geom_point(data=plotData3, aes(x=xMP, y=yMP, colour=Group), shape=4, size=5, show_guide=FALSE, na.rm=T)
    
    p <- addTableToPlot(plotObj=p, tableDF=tableDF, meltVar="condition", clrs=plotCols)
    
    ## Print plot to PDF:
    if (!is.null(filename)) ggsave(filename=filename, plot=p, width=20, height=25, units="cm")    
    return(NULL)
  }
}
