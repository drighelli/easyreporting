
function(input, output, session) {
    library(easyreporting)
    library(ggplot2)
    source(system.file("script/plotFunctions.R",
                       package="easyreporting"))
    
    traceAndPlotVolcano <- function(threshold, er, comment)
    {
        
        mkdCodeChunkTitledCommented(object=er, title="Volcano Plot", 
                                level=1,
                                codeMsg=paste0(" degs <- readRDS(\"degs.RDS\")\nVolcanoPlot(degs=degs, threshold=",
                                           threshold,")"),
                                commentMsg=comment, 
                                sourceFilesList=system.file("script/plotFunctions.R", package="easyreporting"))
        
        degs <- readRDS("degs.RDS")
        vplot <- VolcanoPlot(degs=degs, threshold=threshold)
        return(vplot)
    }
    ###########
    
    vr <- easyreporting(filenamePath=file.path(getwd(), "report"), 
                        title="volcano_report",
                        author=c("Dario Righelli"))
    observeEvent(input$do, {
        output$plot1 <- 
            renderPlot({
                
                traceAndPlotVolcano(threshold=input$slider_pval, 
                                    er=vr, 
                                    comment=input$comment_text)
            })
    })
    
    observeEvent(input$do2, {
        compile(vr)
        browseURL("report.html")
    })
    
}



