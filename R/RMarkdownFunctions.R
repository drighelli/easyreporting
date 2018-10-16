## functions to put in a class
#
# global.chunk.eval <- TRUE
# global.chunk.echo <- TRUE
# global.chunk.warn <- FALSE
# global.chunk.message <- FALSE

#' R6MarkDownClass <- R6::R6Class("r6markdown",
#'     public=list(
#'         initialize=function(filenamepath, title=NULL, author=NULL)
#'         {
#'             private$initReportFilename(filenamepath=filenamepath,
#'                                        mainTitle=title, author=author)
#'         }
#'     ),
#'     private=list(
#'         initReportFilename=initReportFilename,
#'         markdownSetGlobalOpts=markdownSetGlobalOpts,
#'         filenamePath=NULL
#'     )
#' )

#
# CheckReportFilename <- function(file.name) {
#   if(is.null(file.name)) {
#     return(Report.Filename)
#   } else {
#     return(file.name)
#   }
# }
#
# InitReportFilename <- function(mainTitle=NULL, file.name=NULL, Report.Path="./", documentType="html") {
#   ## @file.name: optional, if not NULL the global variable Report.Filename will be used
#   if(!is.null(file.name)) {
#     report.filename <- file.path(Report.Path, file.name)
#     if(!file.exists(report.filename)) {
#       file.create(report.filename)
#
#         header <-paste0(
# "---
# title: \"", mainTitle, "\"
# date: \"`r Sys.Date()`\"
# output: rmarkdown::html_document
# ---")
#         base::write(header, file = report.filename, ncolumns = if(is.character(header)) 1 else 5, append = TRUE, sep = "\n")
#     }
#   }else{
#     stop(paste0("Error: report file name is NULL!"))
#   }
#   return(report.filename)
# }

MarkdownFirstMessageSession <- function(main.interface.name, functionality.name, message=NULL, file.name=NULL) {
  ## @message: an optional additional message to base::write in the report
  ## @file.name: optional, if not NULL the global variable Report.Filename will be used
  file.name <- CheckReportFilename(file.name)

  self.message <- paste0( "* In the *", main.interface.name, "* section you choose the **", functionality.name,"** section at `", Sys.time(), "`")
  base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
  if(!is.null(message)) {
    base::write(message, file = file.name, ncolumns = if(is.character(message)) 1 else 5, append = TRUE, sep = "\n")
  }
}

#
# MarkdownVariableAssignmentMessage <- function(variable.name, real.variable, file.name=NULL) {
#   ## @file.name: optional, if not NULL the global variable Report.Filename will be used
#   file.name <- CheckReportFilename(file.name)
#   self.message <- paste0(variable.name, " <- \"", real.variable,"\"")
#   # print(self.message)
#   base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
# }
#
# MarkdownRCodeChunkStart <- function(eval=global.chunk.eval, echo=global.chunk.echo, warning=global.chunk.warn, loading=global.chunk.message, file.name=NULL, source.files.list=NULL) {
#   file.name <- CheckReportFilename(file.name)
#   self.message <- paste0("```{r eval=", eval, ", echo=", echo, ", warning=", warning, ", message=", loading,"}\n")
#   base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
#   if(!is.null(source.files.list)) {
#     MarkdownSourceFiles(source.files.list)
#   }
#
# }
#
# MarkdownRCodeChunkEnd <- function(file.name=NULL) {
#   file.name <- CheckReportFilename(file.name)
#   self.message <- paste0("```\n")
#   base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
# }

MarkdownRCodeChunkComplete <- function(message, eval=global.chunk.eval, echo=global.chunk.echo, warning=global.chunk.warn,  loading=global.chunk.message, file.name=NULL, source.files.list=NULL)
{
    file.name <- CheckReportFilename(file.name)
    MarkdownRCodeChunkStart(eval=eval, echo=echo, warning=warning, file.name=file.name, source.files.list=source.files.list)
    MarkdownGeneralMessage(message, file.name)
    MarkdownRCodeChunkEnd(file.name)
}

MarkdownParameterMessage <- function(..., message=NULL, file.name=NULL ) {
  ## @... : a named list of parameters to log in the report
  ## @message: an optional additional message -> no checks on it!
  ## @file.name: optional, if not NULL the global variable Report.Filename will be used
  file.name <- CheckReportFilename(file.name)
  self.message <- paste0("## Selected parameters values: \n")
  for(i in 1:length(...)) {
    self.message <- paste0(self.message, "#", names(...)[i], "<- '", ...[[i]], "' \n")
  }
  base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
  if(!is.null(message)) {
    base::write(message, file = file.name, ncolumns = if(is.character(message)) 1 else 5, append = TRUE, sep = "\n")
  }

  #base::write(message, file = file.name, ncolumns = if(is.character(message)) 1 else 5, append = TRUE, sep = "\n")

}
#
# MarkdownSourceFiles <- function(..., file.name=NULL) {
#   file.name <- CheckReportFilename(file.name)
#   self.message <- ""
#   for(i in 1:length(...)) {
#     self.message <- paste0(self.message, "source(\"", file.path(getwd(), ...[[i]]), "\")\n")
#   }
#   base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
#   # print(self.message)
# }

MarkdownLoadCachedObject <- function(db.name.string, object.name, object.string, file.name=NULL) {
  file.name <- CheckReportFilename(file.name)

  self.message <- paste0(object.name, " <- LoadCachedObject(", db.name.string, ", \"", object.string,"\")\n")
  base::write(self.message, file = file.name, ncolumns = if(is.character(self.message)) 1 else 5, append = TRUE, sep = "\n")
  # print(self.message)
}


