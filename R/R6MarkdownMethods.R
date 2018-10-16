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
    output: rmarkdown::", documentType, "_document\n---\n")

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
                    ")\n```\n")

    base::write(options, file=private$filenamePath,
                append=TRUE, sep="\n")

}

#' mkdTitle
#'
#' @param title
#' @param level
#'
#' @return
#' @export
#'
#' @examples
mkdTitle <- function(title, level=1)
{
    if(!is.character(title)) stop("You can enter only string values for title!")
    if(level > 6) stop("You can use at last level!")

    message <- paste0(
        strrep("#", times=level),
        " ",
        title
    )
    base::write(message, file=self$getReportFilename(),
                ncolumns=if(is.character(message)) 1 else 5,
                append=TRUE,
                sep="\n")
}

#' mkdGeneralMsg
#'
#' @param message the message to append to the report
#'
#' @return
#' @export
#'
#' @examples
mkdGeneralMsg <- function(message)
{
    base::write(x=paste0("\n", message, "\n"), file=self$getReportFilename(),
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
                        includeFlag=TRUE
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
                           includeFlag=TRUE)
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
    rmarkdown::render(self$getReportFilename())
}


#' Title
#'
#' @param variable.name
#' @param variable.object.name
#' @param show
#'
#' @return
#' @export
#'
#' @examples
mkdVariableAssignment <- function(variable.name, variable.object.name,
                                show=FALSE)
{
    self.message <- paste0(variable.name, " <- \`", variable.object.name,"\`\n")
    if(show) self.message <- paste0(self.message, "print(", variable.name,")\n")
    # print(self.message)
    base::write(self.message,
                file=self$getReportFilename(),
                ncolumns=if(is.character(self.message)) 1 else 5,
                append=TRUE,
                sep = "\n")
}


#' Title
#'
#' @param optionsList
#' @param source.files.list
#'
#' @return
#' @export
#'
#' @examples
mkdCodeChunkSt <- function(optionsList=self$getOptionsList(),
                        source.files.list=NULL)
{
    self.message <- paste0("```{r eval=", optionsList$evalFlag,
                            ", echo=", optionsList$echoFlag,
                            ", warning=", optionsList$warningFlag,
                            ", message=", optionsList$showMessages,
                            ", include=", optionsList$includeFlag,
                            ", cache=", optionsList$cacheFlag,
                            "}\n")
    base::write(self.message,
                file=self$getReportFilename(),
                ncolumns=if(is.character(self.message)) 1 else 5,
                append=TRUE,
                sep="\n")

    if(!is.null(source.files.list))
    {
        files <- list(source.files.list)
        self.message <- ""
        for(i in 1:length(files))
        {
            self.message <- paste0(self.message,
                                   "source(\"",
                                   file.path(getwd(), files[[i]]),
                                   "\")\n")
        }
        base::write(self.message,
                    file=self$getReportFilename(),
                    ncolumns=if(is.character(self.message)) 1 else 5,
                    append=TRUE,
                    sep="\n")
    }
    message("Please remember to close the Code Chunk!
Just invoke mkdCodeChunkEnd() once you complete your function calling :)")
}

#' mkdSourceFiles
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
mkdSourceFiles <- function(...)
{
    files <- list(...)
    self.message <- ""
    for(i in 1:length(files))
    {
        self.message <- paste0(self.message,
                            "source(\"",
                            file.path(getwd(), files[[i]]),
                            "\")\n")
    }
    base::write(self.message,
                file=self$getReportFilename(),
                ncolumns=if(is.character(self.message)) 1 else 5,
                append=TRUE,
                sep="\n")
}


#' mkdCodeChunkEnd
#'
#' @param self$getReportFilename()
#'
#' @return
#' @export
#'
#' @examples
mkdCodeChunkEnd <- function()
{
    self.message <- paste0("```\n")
    base::write(self.message,
            file=self$getReportFilename(),
            ncolumns=if(is.character(self.message)) 1 else 5,
            append=TRUE,
            sep="\n")
}



#' Title
#'
#' @param message
#' @param optionsList
#' @param source.files.list
#'
#' @return
#' @export
#'
#' @examples
mkdCodeChunkComplete <- function(message, optionsList=self$getOptionsList(),
                                source.files.list=NULL)
{
    self$mkdCodeChunkSt(optionsList=optionsList,
                    source.files.list=source.files.list)
    self$mkdGeneralMsg(message)
    self$mkdCodeChunkEnd()
}

