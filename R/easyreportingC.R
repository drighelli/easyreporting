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
#' @return an easyreporting class instance
#' @export
#'
#' @examples
#' 
easyreporting <- setClass(Class="easyreporting", 
        representation=representation(
             ## attributes
            filenamePath="character",
            title="character",
            author="character",
            documentType="character",
            optionsList="list"
        ),
        prototype=prototype(
            documentType="html",
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

setMethod(f="initialize",
    signature="easyreporting",
    definition=function(.Object, filenamePath, title, author, optionList=NULL)
    {
        .Object <- initReportFilename(object=.Object, filenamepath=filenamePath, 
                        title=title, author=author, optionList=optionList)
        return(.Object)
    }
)





