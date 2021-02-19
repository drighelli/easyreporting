#' easyreporting
#' @rdname easyreporting-class
#' @aliases easyreporting-class
#' @slot filenamePath the path with the name of the rmarkdown.
#' @slot title the title of the report section.
#' @slot author the author(s) of the report.
#' @slot documentType the type of the final document (fixed to "html").
#' @slot optionList a list of options for the general rmarkdown document.
#' @exportClass easyreporting
easyreporting <- setClass(Class="easyreporting", 
    representation=representation(
         ## attributes
        filenamePath="character",
        title="character",
        author="character",
        documentType="character",
        bibfile="character",
        optionList="list"
        
    ),
    prototype=prototype(
        documentType="rmarkdown::html_document",
        optionList=list(
            cache=TRUE,
            eval=TRUE,
            echo=TRUE,
            warning=FALSE,
            showMessages=FALSE,
            include=TRUE,
            collapse=FALSE,
            purl=TRUE,
            error=TRUE,
            message=TRUE,
            highlight=TRUE,
            prompt=FALSE,
            strip.white=TRUE,
            tidy=FALSE
        )
    )
)

setMethod(f="initialize",
    signature="easyreporting",
    definition=function(.Object, filenamePath, title, author, optionList=NULL, 
                        documentType="rmarkdown::html_document", bibfile="")
    {
        .Object <- initReportFilename(object=.Object, filenamepath=filenamePath, 
                        title=title, author=author, optionList=optionList, 
                        documentType=documentType, bibfile=bibfile)
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
#' @param author(s) the author of the report.
#' @param optionList a list of options for the general rmarkdown document.
#'
#' @return an S4 easyreporting class instance
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author="It's me")
easyreporting <- function(filenamePath, title, author, optionList=NULL,
                        documentType="rmarkdown::html_document", bibfile="")
{
    new(Class="easyreporting", filenamePath=filenamePath, title=title, 
        author=author, bibfile=bibfile, optionList=optionList)
}




