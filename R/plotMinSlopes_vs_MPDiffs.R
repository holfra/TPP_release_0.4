#' @title Create 'bottle plot' and histogram of minimal slopes
#' @param mpDiffs numeric vector of melting point differences.
#' @param minSlopes numeric vector of minimal slopes.
#' @param pValues numeric vector of p-values
#' @param expName1 name of the first experiment for the plot title.
#' @param expName2 name of the second experiment for the plot title.
#' @param plotTheme ggplot theme
#' @param path destination where to save the plot (optional)
#' @importFrom gridExtra arrangeGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom grid gpar
#' @importFrom grid textGrob
plotMinSlopes_vs_MPDiffs <- function(mpDiffs, minSlopes, pValues, expName1, expName2, 
                                     plotTheme=tppDefaultTheme(), path=NULL){  
  theme_set(plotTheme)
  
  ## Create dataframe to be passed to ggplot function:
  plotDf <- data.frame(mpDiffs=mpDiffs, minSlopes=minSlopes, pValues=pValues)
  ## Only include proteins, for which p-values could be computed 
  ## (criterion: R2 > 0.8 (both columns) + Plateau < 0.3 (Vehicle)):
  plotDf <- subset(plotDf, !is.na(pValues))
  
  signfTest <- rep(NA, nrow(plotDf))
  signfTest[plotDf$pValues>0.05] <- "p_adj > 0.05"
  signfTest[plotDf$pValues<=0.05] <- "p_adj <= 0.05"
  plotDf$signfTest <- factor(signfTest, levels=c("p_adj > 0.05", "p_adj <= 0.05"))
  
  yLimVec <- c(0, -1.5)
  scatterPlot <- ggplot()
  scatterPlot <- scatterPlot + scale_alpha(range = c(0.3, 1))
  scatterPlot <- scatterPlot + geom_point(data=plotDf, aes(x=mpDiffs, y=minSlopes, color=signfTest), size=3)
  scatterPlot <- scatterPlot + scale_x_continuous(limits=c(-15,15),breaks=seq(-10,10,by=1), labels=c(-10, rep("",4), -5, rep("",4), 0, rep("",4), 5, rep("",4), 10)) 
  scatterPlot <- scatterPlot + ylim(yLimVec)
  scatterPlot <- scatterPlot + xlab(paste('Melting point difference [\U00B0', 'C]', sep='')) + ylab("Minimal slope")
  scatterPlot <- scatterPlot + theme(legend.position=c(0.85,0.85), legend.title=element_blank())
  
  histPlot <- ggplot()
  histPlot <- histPlot + geom_histogram(data=plotDf, aes(x=minSlopes), binwidth=abs(diff(yLimVec))/100) 
  histPlot <- histPlot + coord_flip()
  histPlot <- histPlot + scale_x_reverse(labels=NULL,limits=yLimVec)
  histPlot <- histPlot + xlab(NULL)
  
  ## Save plot:
  if (!is.null(path)){
    if (!file.exists(path)) dir.create(path, recursive=T)
    plotFile <- paste("MeltingCurveComparisons_", expName1, "_vs_", expName2, ".pdf", sep="")
    pdf(file=file.path(path, plotFile), width=8, height=8)
  }
  grid.arrange(arrangeGrob(scatterPlot, histPlot, ncol=2, widths=c(3,1)), heights=c(1,3),
               main = textGrob(paste(expName1, "vs.", expName2),gp=gpar(fontsize=20,fontface='bold'), just="top"))
  if (!is.null(path)){
    dev.off()
  }
}