
erGUIVolcano <- function()
{
    appDir <- system.file("shiny_examples", "volcano_app", package="easyreporting")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `easyreporting`.", call.=FALSE)
    }
    
    shiny::runApp(appDir, display.mode="normal")
}
