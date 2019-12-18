#' easyreporting
#' @description An S4 class for managing rmarkdown report.
#' Each instance describes an rmarkdown file.
#' @docType class
#' @rdname easyreporting-class
#' @aliases easyreporting-class
#' @slot filenamePath the path with the name of the rmarkdown.
#' @slot title the title of the report section.
#' @slot author the author of the report.
#' @slot documentType the type of the final document (fixed to "html").
#' @slot optionList a list of options for the general rmarkdown document.
#'
#' @return an easyreporting class instance
#' @exportClass easyreporting
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

## Constructor
#' @description An S4 class for managing rmarkdown report.
#' Each instance describes an rmarkdown file.
#' @importFrom  methods new
#' @rdname easyreporting-class
#' @param filenamePath the path with the name of the rmarkdown.
#' @param title the title of the report section.
#' @param author the author of the report.
#' @param optionList a list of options for the general rmarkdown document.
#'
#' @return an S4 easyreporting class instance
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author="It's me")
easyreporting <- function(filenamePath, title, author, optionList=NULL)
{
    new(Class="easyreporting", filenamePath=filenamePath, title=title, 
        author=author, optionList=optionList)
}




