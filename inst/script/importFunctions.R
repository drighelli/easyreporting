
importData <- function(xlsFile)
{
    require(readxl)
    geneCounts <- as.data.frame(read_xlsx(xlsFile))
    rownames(geneCounts) <- geneCounts[,1]
    geneCounts <- geneCounts[,c(2:7)]
    return(geneCounts)
}
