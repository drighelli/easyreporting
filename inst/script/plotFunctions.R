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