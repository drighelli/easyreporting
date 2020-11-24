
function(input, output, session) {
  library(easyreporting)
  library(ggplot2)
  source(system.file("script/plotFunctions.R",
                     package="easyreporting"))
  # MAedgeRMAPlotEx <- function(degList, threshold)
  # {
  #   # for (i in seq_along(degList)) 
  #   # {
  #   degenes <- degList$FDR < threshold
  #   
  #   with(degList, plot(logCPM, logFC, pch=16, cex=1, main=names(degList)))
  #   with(degList, points(logCPM[degenes], logFC[degenes], col='red', pch=16, cex=1))
  #   # }
  # }
  # 
  # traceAndPlotMAPlot <- function(degList, er, threshold)
  # {
  #   mkdCodeChunkTitledCommented(er, title="Recursive Tracing Function", level=2,
  #                               codeMsg="MAedgeRMAPlotEx(degList=`degList`)")
  #   MAedgeRMAPlotEx(degList=degList)
  # }
  traceAndPlotVolcano <- function(threshold, er, traceFlag, comment)
  {
   
    if(traceFlag)
    {
      
      mkdCodeChunkTitledCommented(object=er, title="Volcano Plot", 
              level=1,
              codeMsg=paste0(" degs <- readRDS(\"degs.RDS\")\nPlotVolcanoPlot(degs=degs, threshold=",
                             threshold,")"),
              commentMsg=comment, sourceFilesList="volcano.R")
    }
    degs <- readRDS("degs.RDS")
    vplot <- PlotVolcanoPlot(degs=degs, threshold=threshold)
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
                            er=vr, traceFlag=input$rr_flag, 
                            comment=input$comment_text)
    })
  })
  
  observeEvent(input$do2, {
    compile(vr)
    browseURL("report.html")
  })

}



