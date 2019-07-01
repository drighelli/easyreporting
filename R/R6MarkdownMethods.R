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

    base::write(header, file=filenamepath,
                append=TRUE, sep="\n")
    private$markdownSetGlobalOpts(optionList=optionsList)
}

#' mkdSetGlobalOpts
#' @description internal function for appending to the report the initial
#' options setup
#'
#' @param optionsList a list of options
#'
#' @return none
#' @internal
#'
#' @importFrom base write
#'
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
#' @description Inserts an rmarkdown title inside the report.
#'
#' @param title a string within the title.
#' @param level a numeric from 1 to 6 (default is 1).
#'
#' @return none
#' @export
#'
#' @importFrom base write
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' rd$mkdTitle("First Level Title")
#' rd$mkdTitle("Second Level Title", level=2)
#'
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
#' @description It appends a general message to the report.
#' Useful for adding natural language comments.
#'
#' @param message the message to append to the report
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' rd$mkdGeneralMsg("Writing a paragraph to describe my code chunk")
mkdGeneralMsg <- function(message)
{
    base::write(x=paste0("\n", message, "\n"), file=self$getReportFilename(),
                ncolumns=if(is.character(message)) 1 else 5,
                append=TRUE,
                sep="\n")
}



#' setOptionsList
#' @description set an optionList to the rmarkdown R6 class
#'
#' @param cacheFlag boolean for caching chunk data (default TRUE)
#' @param evalFlag boolean for evaluating the code chunk in the compiled version
#' (default TRUE)
#' @param echoFlag boolean for showing the code chunk (default TRUE)
#' @param warningFlag boolean for showing the chunk warnings (default FALSE)
#' @param showMessages boolean for showing the chunk warnings in compiled
#' version (default FALSE)
#' @param includeFlag boolean for including the code chunk in the compiled
#' version (default TRUE)
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' ## setting default option
#' rd$setOptionsList()
#'
#' ## modifying only some options
#' rd$setOptionsList(warningFlag=TRUE, showMessages=TRUE, includeFlag=TRUE)
#'
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
#' @description returns the optionsList from the easyreporting class
#'
#' @return a list of options
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- rd$getOptionsList()
#'
getOptionsList <- function()
{
    return(private$optionsList)
}


#' maketOptionsList
#' @description makes an list of rmarkdown options
#'
#' @param cacheFlag boolean for caching chunk data (default TRUE)
#' @param evalFlag boolean for evaluating the code chunk in the compiled version
#' (default TRUE)
#' @param echoFlag boolean for showing the code chunk (default TRUE)
#' @param warningFlag boolean for showing the chunk warnings (default FALSE)
#' @param showMessages boolean for showing the chunk warnings in compiled
#' version (default FALSE)
#' @param includeFlag boolean for including the code chunk in the compiled
#' version (default TRUE)
#'
#' @return list of rmarkdown options
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- rd$makeOptionsList()
makeOptionsList <- function(cacheFlag=TRUE,
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
mkdCodeChunkComplete <- function(message,
                                optionsList=self$getOptionsList(),
                                source.files.list=NULL)
{
    self$mkdCodeChunkSt(optionsList=optionsList,
                        source.files.list=source.files.list)
    self$mkdGeneralMsg(message)
    self$mkdCodeChunkEnd()
}


#' mkdCodeChunkCommented
#'
#' @param comment
#' @param message
#' @param optionsList
#' @param source.files.list
#'
#' @return
#' @export
#'
#' @examples
mkdCodeChunkCommented <- function(commentMsg=NULL, message,
                                 optionsList=self$getOptionsList(),
                                 source.files.list=NULL)
{
    if(!is.null(commentMsg))
    {
        self$mkdGeneralMsg(commentMsg)
    }
    self$mkdCodeChunkComplete(message=message, optionsList=optionsList,
                        source.files.list=source.files.list)
}



