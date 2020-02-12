
importData <- function(xlsFile)
{
    geneCounts <- as.data.frame(readxl::read_xlsx(xlsFile))
    rownames(geneCounts) <- geneCounts[,1]
    geneCounts <- geneCounts[,c(2:7)]
    return(geneCounts)
}
