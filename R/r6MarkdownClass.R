
#' easyreporting
#' @description An R6 class for managing rmarkdown report.
#' Each instance describes an rmarkdown file.
#' @docType class
#' @usage easyreporting(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' @param filenamepath the path with the name of the rmarkdown.
#' @param title the title fo the report.
#' @param author the author of the report.
#'
#' @return an easyreporting R6 class instance
#' @importFrom R6 R6Class
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
easyreporting <- R6::R6Class("easyreporting",
   public=list(
       initialize=function(filenamepath, title=NULL, author=NULL)
       {
            private$initReportFilename(filenamepath=filenamepath,
                                        mainTitle=title,
                                        author=author,
                                        optionsList=private$optionsList)
       },
       setOptionsList=setOptionsList,
       getOptionsList=getOptionsList,
       mkdGeneralMsg=mkdGeneralMsg,
       mkdTitle=mkdTitle,
       compile=compile,
       getReportFilename=getReportFilename,
       mkdVariableAssignment=mkdVariableAssignment,
       mkdCodeChunkSt=mkdCodeChunkSt,
       mkdCodeChunkEnd=mkdCodeChunkEnd,
       mkdSourceFiles=mkdSourceFiles,
       mkdCodeChunkComplete=mkdCodeChunkComplete,
       mkdCodeChunkCommented=mkdCodeChunkCommented
   ),
   private=list(
        ## methods
        initReportFilename=initReportFilename,
        markdownSetGlobalOpts=mkdSetGlobalOpts,

        ## attributes
        filenamePath=NULL,
        optionsList=list(
            cacheFlag=TRUE,
            evalFlag=TRUE,
            echoFlag=TRUE,
            warningFlag=FALSE,
            showMessages=FALSE,
            includeFlag=TRUE
        )
    )
)





