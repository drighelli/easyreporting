#' initReportFilename
#' @description internal funtion usefull to init the report for the first time.
#'
#' @param filenamepath the name of the report with or without the path
#' @param mainTitle the title of the report
#' @param author the name of the report author
#' @param documentType type of report final document. (html is default)
#'
#' @internal
#' @importFrom tools file_ext
#' @importFrom base write
#' @return none
initReportFilename=function(filenamepath=NULL, mainTitle=NULL,
                            author=NULL, documentType="html",
                            optionsList=NULL)
{
    if(is.null(filenamepath)) stop("Report file name is NULL!")
    if(is.null(mainTitle)) stop("Please provide a title for the document!")
    documentType <- match.arg(documentType)
    if(!file.exists(filenamepath))
    {
        if(tools::file_ext(filenamepath) != "Rmd")
        {
            filenamepath <- paste0(filenamepath, ".Rmd")
        }
        file.create(filenamepath)
    }

    private$filenamePath <- filenamepath

    if(length(author) > 1)
    {
        message("Only the first author will be put inside the report.")
        author <- author[1]
    }
    ## do not indent!
    header <- paste0(
    "---
    title: \"", mainTitle, "\"
    author: \"", author, "\"
    date: \"`r Sys.Date()`\"
    output: rmarkdown::", documentType, "_document
    \n---\n"
    )

    base::write(header, file = filenamepath,
                append = TRUE, sep = "\n")
    private$markdownSetGlobalOpts(optionList=optionsList)
}

#' markdownSetGlobalOpts
#'
#' @param optionsList
#'
#' @return
#' @export
#'
#' @examples
mkdSetGlobalOpts <- function(optionList)
{
    options <- paste0("```{r global_options, include=FALSE}\n",
                    "knitr::opts_chunk$set(",
                    "eval=", optionList$evalFlag,
                    ", echo=", optionList$echoFlag,
                    ", warning=", optionList$warningFlag,
                    ", message=", optionList$showMessages,
                    ", include=", optionList$includeFlag,
                    ", cache=", optionList$cacheFlag,
                    ")\n```")
    base::write(options, file = private$filenamePath,
                append = TRUE, sep = "\n")

}

#' Title
#'
#' @param title
#' @param level
#' @param file.name
#'
#' @return
#' @export
#'
#' @examples
mkdTitle <- function(title, level=1, file.name=NULL)
{
    if(!is.character(title)) stop("You can enter only string values for title!")
    if(level > 6) stop("You can use at last level!")

    file.name <-  if(is.null(file.name)) self$getReportFilename() else file.name

    message <- paste0(
        strrep("#", times=level),
        " ",
        title
    )
    base::write(message, file=file.name,
                ncolumns=if(is.character(message)) 1 else 5,
                append=TRUE,
                sep="\n")
}

#' mkdGeneralMsg
#'
#' @param message the message to append to the report
#' @param file.name optional, if not NULL the global class attribute
#' name will be used
#'
#' @return
#' @export
#'
#' @examples
mkdGeneralMsg <- function(message, file.name=NULL)
{
    file.name <-  if(is.null(file.name)) self$getReportFilename() else file.name

    base::write(x=paste0("\n", message), file=file.name,
                ncolumns=if(is.character(message)) 1 else 5,
                append=TRUE,
                sep="\n")
}



#' setOptionsList
#'
#' @param cacheFlag
#' @param evalFlag
#' @param echoFlag
#' @param warningFlag
#' @param showMessages
#' @param includeFlag
#'
#' @return
#' @export
#'
#' @examples
setOptionsList <- function(cacheFlag=TRUE,
                        evalFlag=TRUE,
                        echoFlag=TRUE,
                        warningFlag=FALSE,
                        showMessages=FALSE,
                        includeFlag=FALSE
                        )
{
    private$optionsList <- list(
        cacheFlag=cacheFlag,
        evalFlag=evalFlag,
        echoFlag=echoFlag,
        warningFlag=warningFlag,
        showMessages=showMessages,
        includeFlag=includeFlag
    )
}

#' getOptionsList
#'
#' @return
#' @export
#'
#' @examples
getOptionsList <- function()
{
    return(private$optionsList)
}


#' maketOptionsList
#'
#' @param cacheFlag
#' @param evalFlag
#' @param echoFlag
#' @param warningFlag
#' @param showMessages
#' @param includeFlag
#'
#' @return
#' @export
#'
#' @examples
maketOptionsList <- function(cacheFlag=TRUE,
                           evalFlag=TRUE,
                           echoFlag=TRUE,
                           warningFlag=FALSE,
                           showMessages=FALSE,
                           includeFlag=FALSE)
{
    return( list(
        cacheFlag=cacheFlag,
        evalFlag=evalFlag,
        echoFlag=echoFlag,
        warningFlag=warningFlag,
        showMessages=showMessages,
        includeFlag=includeFlag
    ))
}

getReportFilename <- function()
{
    return(private$filenamePath)
}

compile <- function()
{
    rmarkdown::render(self$getReportFilename)
}
