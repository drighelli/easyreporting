
importData <- function(xlsFile)
{
    geneCounts <- as.data.frame(readxl::read_xlsx(xlsFile))
    rownames(geneCounts) <- geneCounts$`Feature ID`
    geneCounts <- geneCounts[,c(2:7)]
    return(geneCounts)
}
