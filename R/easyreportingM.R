#' initReportFilename
#' @description internal funtion usefull to init the report for the first time.
#'
#' @param filenamepath the name of the report with or without the path
#' @param mainTitle the title of the report
#' @param author the name of the report author
#' @param documentType type of report final document. (html is default)
#'
#' @keywords internal
#' @importFrom tools file_ext
#'
#' @return none
setMethod(f="initReportFilename", signature="easyreporting",
        definition=function(object, 
                filenamepath=NULL, title=NULL,
                author=NULL, optionList=NULL)
{
    if(is.null(filenamepath)) stop("Report file name is NULL!")
    if(is.null(title)) stop("Please provide a title for the document!")
    if(object@documentType != "html") stop("documentType has to be HTML!")
    
    if(!file.exists(filenamepath))
    {
        if(tools::file_ext(filenamepath) != "Rmd")
        {
            filenamepath <- paste0(filenamepath, ".Rmd")
        }
        file.create(filenamepath)
    }

    object@filenamePath <- filenamepath

    if(length(author) > 1)
    {
        message("Only the first author will be put inside the report.")
        author <- author[1]
    }
    object@author <- author
    object@title <- title
    ## do not indent!
    header <- paste0(
        "---
    title: \"", object@title, "\"
    author: \"", object@author, "\"
    date: \"`r Sys.Date()`\"
    output: rmarkdown::", object@documentType, "_document\n---\n")

    base::write(header, file=filenamepath,
                append=TRUE, sep="\n")
    mkdSetGlobalOpts(object=object, optionList=optionList)
})

#' mkdSetGlobalOpts
#' @description internal function for appending to the report the initial
#' options setup
#'
#' @param optionsList a list of options
#'
#' @return none
#' @keywords internal
#'
setMethod(f="mkdSetGlobalOpts", signature="easyreporting",
    definition=function(object, optionList=list())
    {
        if(!is.null(optionList)) object@optionsList <- optionList
        options <- paste0("```{r global_options, include=FALSE}\n",
                          "knitr::opts_chunk$set(",
                          "eval=", object@optionsList$evalFlag,
                          ", echo=", object@optionsList$echoFlag,
                          ", warning=", object@optionsList$warningFlag,
                          ", message=", object@optionsList$showMessages,
                          ", include=", object@optionsList$includeFlag,
                          ", cache=", object@optionsList$cacheFlag,
                          ")\n```\n")
        
        base::write(options, file=object@filenamePath,
                    append=TRUE, sep="\n")
    }
)
# mkdSetGlobalOpts <- function(optionList=list())
# {
#     if(length(optionList)==0) stop("Please provide a list of options!")
#     options <- paste0("```{r global_options, include=FALSE}\n",
#                     "knitr::opts_chunk$set(",
#                     "eval=", optionList$evalFlag,
#                     ", echo=", optionList$echoFlag,
#                     ", warning=", optionList$warningFlag,
#                     ", message=", optionList$showMessages,
#                     ", include=", optionList$includeFlag,
#                     ", cache=", optionList$cacheFlag,
#                     ")\n```\n")
# 
#     base::write(options, file=private$filenamePath,
#                 append=TRUE, sep="\n")
# 
# }

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
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' rd$mkdTitle("First Level Title")
#' rd$mkdTitle("Second Level Title", level=2)
#'
mkdTitle <- function(object, title, level=1)
{
    if(!is.character(title)) stop("You can enter only string values for title!")
    if(level > 6) stop("You can use at last level 6!")

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
#' optList <- makeOptionsList()
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

#' getReportFilename
#' @description returns the report filename with path
#' @return a string of report file name with path
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' rep <- rd$getReportFilename()
getReportFilename <- function()
{
    return(private$filenamePath)
}


#' compile
#' @description compiles the rmarkdown file
#' @return none
#' @export
#' @importFrom rmarkdown render
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' rd$compile()
compile <- function()
{
    rmarkdown::render(self$getReportFilename())
}


#' mkdVariableAssignment
#' @description it includes a variable assignment in the report.
#' NB: a call to the "mkdCodeChunkSt" has to be done before using it.
#'
#' @param variable.name a string indicating the name of the variabe to store in
#' the report. (This can be changed here, but further uses of the variable needs
#' to take into account the variable name change).
#' @param variable.object.name the name of the already existing variable. (This
#' cannot be canged.)
#' @param show a boolean indicating if to show the message before writing it
#' into the rmardown file.
#'
#' @return none
#' @export
#'
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' ## leaving the default options to the code chunk
#' rd$mkdCodeChunkSt()
#' ## adding a variable assignement
#' variable <- 1
#' rd$mkdVariableAssignment("variable", "variable", show=TRUE)
#' rd$mkdCodeChunkEnd()
#'
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


#' mkdCodeChunkSt
#' @description it creates a code chunk start. A list of options and files to
#' source  for the chunk can optionally be passed to the function.
#' @param optionsList a list of options
#' @param sourceFilesList a list of files that can be sourced inside the code
#' chunk.
#' @param isComplete a flag determining if the chunk is already a complete chunk
#'
#' @return none
#' @export
#'
#'
#' @examples
#'  rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' ## no options
#' rd$mkdCodeChunkSt()
#' ## just leaving empty
#' rd$mkdCodeChunkEnd()
#'
#' ## setting options
#' optList <- makeOptionsList(includeFlag=TRUE)
#' rd$mkdCodeChunkSt(optionsList=optList)
#' ## just leaving empty
#' rd$mkdCodeChunkEnd()
#'
mkdCodeChunkSt <- function(optionsList=self$getOptionsList(),
                           sourceFilesList=NULL, isComplete=FALSE)
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

    if(!is.null(sourceFilesList))
    {
        files <- list(sourceFilesList)
        self.message <- ""
        for(i in seq_along(files)(files))
        {
            self.message <- paste0(self.message,
                                   "source(\"",
                                   files[[i]],
                                   "\")\n")
        }
        base::write(self.message,
                    file=self$getReportFilename(),
                    ncolumns=if(is.character(self.message)) 1 else 5,
                    append=TRUE,
                    sep="\n")
    }
    if(!isComplete)
    {
        message(paste0("Please remember to close the Code Chunk!\n",
                "Just invoke mkdCodeChunkEnd() once you complete your",
                " function calling :)"))
    }
}

#' mkdSourceFiles
#' @description includes a list of source files inside the rmarkdown
#'
#' @param ... a list of files to source
#'
#' @return none
#'
#' @keywords internal
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
#' @description it creates a code chunk end. Always use it after a
#' mkdCodeChunkSt()
#'
#' @return none
#'
#' @export
#'
#' @examples
#'  rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' rd$mkdCodeChunkSt()
#' ## just leaving empty
#' rd$mkdCodeChunkEnd()
mkdCodeChunkEnd <- function()
{
    self.message <- paste0("```\n")
    base::write(self.message,
            file=self$getReportFilename(),
            ncolumns=if(is.character(self.message)) 1 else 5,
            append=TRUE,
            sep="\n")
}



#' mkdCodeChunkComplete
#' @description it creates a complete code chunk.
#' @param message a string containing a function call or the entire code chunk
#' to trace.
#' @param optionsList a list of options.
#' @param sourceFilesList a list of files to source.
#'
#' @return none
#' @export
#'
#' @examples
#'  rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' rd$mkdCodeChunkComplete(message="a <- 1\nb <- 2\nc <- a+b\n print(c)")
mkdCodeChunkComplete <- function(message,
                                optionsList=self$getOptionsList(),
                                sourceFilesList=NULL)
{
    self$mkdCodeChunkSt(optionsList=optionsList,
                        sourceFilesList=sourceFilesList,
                        isComplete=TRUE)
    self$mkdGeneralMsg(message)
    self$mkdCodeChunkEnd()
}

#' mkdCodeChunkCommented
#' @description it creates a complete code chunk, adding a natural language
#' comment before of it.
#' @param commentMsg a string with the natural language comment for the chunk.
#' @param codeMsg a string within the code.
#' @param optionsList a list of options (default is the class options).
#' @param sourceFilesList a optional list of files to source inside the chunk.
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting$new(filenamepath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)
#' rd$mkdCodeChunkCommented(
#'                 commentMsg="This is the comment of the following code chunk",
#'                 codeMsg="a <- 1\nb <- 2\n(c <- a+b)\n", optionsList=optList,
#'                 sourceFilesList=NULL)
mkdCodeChunkCommented <- function(commentMsg=NULL, codeMsg,
                                 optionsList=self$getOptionsList(),
                                 sourceFilesList=NULL)
{
    if(!is.null(commentMsg))
    {
        self$mkdGeneralMsg(commentMsg)
    }
    self$mkdCodeChunkComplete(message=codeMsg, optionsList=optionsList,
                              sourceFilesList=sourceFilesList)
}



