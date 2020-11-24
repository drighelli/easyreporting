MAedgeRMAPlotEx <- function(degList)
{
    for (i in seq_along(degList)) 
    {
        degenes <- degList[[i]]$FDR < 0.01
        with(degList[[i]], plot(logCPM, logFC, pch=16, cex=0.2, main=names(degList)[i]))
        with(degList[[i]], points(logCPM[degenes], logFC[degenes], col='red', pch=16, cex=0.2))
    }
}

traceAndPlotMAPlot <- function(degList, er)
{
    mkdCodeChunkTitledCommented(er, title="Recursive Tracing Function", level=2,
                                codeMsg="MAedgeRMAPlotEx(degList=`degList`)")
    MAedgeRMAPlotEx(degList=degList)
}

VolcanoPlot <- function(degs, threshold=0.05) 
{
    library(ggplot2)
    sig <- sum(degs$FDR < threshold)
    notsig <- dim(degs)[1] - sig
    degs$significance <- paste0("p-adj >= ", threshold, " [", notsig,"]")
    degs$significance[degs$FDR < threshold] <- paste0("p-adj < ", threshold, " [",sig,"]")
    
    ggp <- ggplot2::ggplot(degs) +
        geom_point(
            aes(x=logFC, y=-log10(FDR), color=significance), size=0.7) + 
        xlab(label=~log[2]~"(FC)") + ylab(label=bquote(~-log[10]~"(padj)")) +
        ggtitle("Volcano Plot")
    
    
    ggp <- ggp + 
        geom_vline(xintercept=0) + 
        geom_vline(xintercept=1, colour="darkgreen", linetype="dashed") + 
        geom_vline(xintercept=-1, colour="darkgreen", linetype="dashed")
    return(ggp)
}
