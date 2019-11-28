#' easyreporting
#' @description An S4 class for managing rmarkdown report.
#' Each instance describes an rmarkdown file.
#' @docType class
#' @importFrom  methods new
#' @slot filenamePath the path with the name of the rmarkdown.
#' @slot title the title fo the report.
#' @slot author the author of the report.
#' @slot documentType the type of the final document (fixed to "html").
#' @slot optionList a list of options for the general rmarkdown document.
#'
#' @return an easyreporting class instance
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author="It's me")
easyreporting <- setClass(Class="easyreporting", 
    representation=representation(
         ## attributes
        filenamePath="character",
        title="character",
        author="character",
        documentType="character",
        optionList="list"
    ),
    prototype=prototype(
        documentType="html",
        optionList=list(
            cacheFlag=TRUE,
            evalFlag=TRUE,
            echoFlag=TRUE,
            warningFlag=FALSE,
            showMessages=FALSE,
            includeFlag=TRUE
        )
    )
)

setMethod(f="initialize",
    signature="easyreporting",
    definition=function(.Object, filenamePath, title, author, optionList=NULL)
    {
        .Object <- initReportFilename(object=.Object, filenamepath=filenamePath, 
                        title=title, author=author, optionList=optionList)
        return(.Object)
    }
)

#' easyreporting
#'
#' @param filenamePath the path with the name of the rmarkdown.
#' @param title the title fo the report.
#' @param author the author of the report.
#' @param optionList a list of options for the general rmarkdown document.
#'
#' @return an easyreporting class object instance
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"),
#'                         optionList=makeOptionsList())
easyreporting <- function(filenamePath, title, author, optionList=NULL)
{
    new(Class="easyreporting", filenamePath=filenamePath, title=title, 
        author=author, optionList=optionList)
}




