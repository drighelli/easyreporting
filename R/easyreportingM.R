#' initReportFilename
#' @description internal funtion usefull to init the report for the first time.
#'
#' @param object an easyreporting class object 
#' @param filenamepath the name of the report with or without the path
#' @param mainTitle the title of the report
#' @param author the name of the report author
#' @param documentType type of report final document. (html is default)
#'
#' @keywords internal
#' @importFrom tools file_ext
#'
#' @return an easyreporting class object 
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
    object <- mkdSetGlobalOpts(object=object, optionList=optionList)
    return(object)
})

#' mkdSetGlobalOpts
#' @description internal function for appending to the report the initial
#' options setup
#'
#' @param object an easyreporting class object
#' @param optionList a list of options
#'
#' @return an easyreporting class object
#' @keywords internal
#'
setMethod(f="mkdSetGlobalOpts", signature="easyreporting",
    definition=function(object, optionList=list())
    {
        if(!is.null(optionList)) object@optionList <- optionList
        options <- paste0("```{r global_options, include=FALSE}\n",
                        "knitr::opts_chunk$set(",
                        "eval=", object@optionList$evalFlag,
                        ", echo=", object@optionList$echoFlag,
                        ", warning=", object@optionList$warningFlag,
                        ", message=", object@optionList$showMessages,
                        ", include=", object@optionList$includeFlag,
                        ", cache=", object@optionList$cacheFlag,
                        ")\n```\n")
        
        base::write(options, file=object@filenamePath,
                    append=TRUE, sep="\n")
        return(object)
    }
)

#' mkdTitle
#'
#' @description Inserts an rmarkdown title inside the report.
#' 
#' @param object an easyreporting class object
#' @param title a string within the title.
#' @param level a numeric from 1 to 6 (default is 1).
#'
#' @return none
#' @export
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' mkdTitle(rd, "First Level Title")
#' mkdTitle(rd, "Sub-Title", level=2)
#'
setMethod(f="mkdTitle", signature="easyreporting",
        definition=function(object, title, level=1)
    {
        if(!is.character(title)) 
            stop("You can enter only string values for title!")
        if(level > 6) stop("You can use at last level 6!")
    
        message <- paste0(
            strrep("#", times=level),
            " ",
            title
        )
        base::write(message, file=getReportFilename(object),
                    ncolumns=if(is.character(message)) 1 else 5,
                    append=TRUE,
                    sep="\n")
    }
)

#' mkdGeneralMsg
#' @description It appends a general message to the report.
#' Useful for adding natural language comments.
#'
#' @param object an easyreporting class object
#' @param message the message to append to the report
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdGeneralMsg(rd, "Writing a paragraph to describe my code chunk")
setMethod(f="mkdGeneralMsg", signature="easyreporting",
    definition=function(object, message)
    {
        base::write(x=paste0("\n", message, "\n"), 
            file=getReportFilename(object),
            ncolumns=if(is.character(message)) 1 else 5,
            append=TRUE, sep="\n")
    }
)


#' mkdGeneralTitledMsg
#' @description It appends a a titled section followed by a general message to 
#' the report. Useful for adding natural language comments.
#' @param object an easyreporting class object
#' @param title the optional title to give to the message section
#' @param level the level (1 to 6) of the title (default is 1)
#' @param message the message to append to the report
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdGeneralTitledMsg(rd, title="Generic SubTitle for this message", level=2,
#'         message="Writing a paragraph to describe my code chunk")
setMethod(f="mkdGeneralTitledMsg", signature="easyreporting",
        definition=function(object, title=NULL, level=1, message)
        {
            if(!is.null(title)) mkdTitle(object, title=title, level=level)
            base::write(x=paste0("\n", message, "\n"), 
                        file=getReportFilename(object),
                        ncolumns=if(is.character(message)) 1 else 5,
                        append=TRUE, sep="\n")
        }
)

#' setOptionsList
#' @description set an optionList to the class
#' 
#' @param object an easyreporting class object
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
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' ## setting default option
#' setOptionsList(rd)
#'
#' ## modifying only some options
#' rd <- setOptionsList(rd, warningFlag=TRUE, 
#'         showMessages=TRUE, includeFlag=TRUE)
#' 
setMethod(f="setOptionsList", signature="easyreporting",
    definition=function(object, 
                        cacheFlag=TRUE,
                        evalFlag=TRUE,
                        echoFlag=TRUE,
                        warningFlag=FALSE,
                        showMessages=FALSE,
                        includeFlag=TRUE
                        )
    {
        object@optionList <- list(
            cacheFlag=cacheFlag,
            evalFlag=evalFlag,
            echoFlag=echoFlag,
            warningFlag=warningFlag,
            showMessages=showMessages,
            includeFlag=includeFlag
        )
        return(object)
    }
)

#' getOptionsList
#'
#' @description returns the optionList from the easyreporting class
#'
#' @param object an easyreporting class object
#'
#' @return a list of options
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report", title="example_report",
#'                         author=c("It's me"))
#' optList <- getOptionsList(rd)
#'
setMethod(f="getOptionsList", signature="easyreporting", 
    definition=function(object)
    {
        return(object@optionList)
    }
)


#' makeOptionsList
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
#' @param object an easyreporting class object
#' @return a string of report file name with path
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' (rep <- getReportFilename(rd))
setMethod(f="getReportFilename", signature="easyreporting", 
    definition=function(object)
    {
        return(object@filenamePath)
    }
)

#' compile
#' @description prints the sessionInfo and compiles the rmarkdown file
#' 
#' @param object an easyreporting class object
#' 
#' @return none
#' @importFrom rmarkdown render
#' @export
#' 
#' @examples
#' rd <- easyreporting(filenamePath="./project_report", title="example_report",
#'                         author=c("It's me"))
#' compile(rd)
#' 
setMethod(f="compile", signature="easyreporting",
    definition=function(object)
    {
        mkdCodeChunkTitledCommented(object=object, title="Session Info", 
            codeMsg="sessionInfo()")
        rmarkdown::render(getReportFilename(object))
    }
)

#' mkdVariableAssignment
#' @description it includes a variable assignment in the report.
#' NB: a call to the "mkdCodeChunkSt" has to be done before using it.
#'
#' @param object an easyreporting class object
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
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' ## leaving the default options to the code chunk
#' mkdCodeChunkSt(rd)
#' ## adding a variable assignement
#' variable <- 1
#' mkdVariableAssignment(rd, "variable", "variable", show=TRUE)
#' mkdCodeChunkEnd(rd)
#'
setMethod(f="mkdVariableAssignment", signature="easyreporting", 
    definition=function(object, variable.name, variable.object.name, show=FALSE)
    {
        self.message <- paste0(variable.name, " <- ", 
                            variable.object.name,"\n")
        if(show) self.message <- paste0(self.message, 
                                    "print(", variable.name,")\n")
        # print(self.message)
        base::write(self.message,
                    file=getReportFilename(object),
                    ncolumns=if(is.character(self.message)) 1 else 5,
                    append=TRUE,
                    sep = "\n")
    }
)


#' mkdCodeChunkSt
#' @description it creates a code chunk start. A list of options and files to
#' source  for the chunk can optionally be passed to the function.
#' @param object an easyreporting class object
#' @param optionList a list of options
#' @param sourceFilesList a list of files that can be sourced inside the code
#' chunk.
#' @param isComplete a flag determining if the chunk is already a complete chunk
#'
#' @return none
#' @export
#'
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' ## no options
#' mkdCodeChunkSt(rd)
#' ## just leaving empty
#' mkdCodeChunkEnd(rd)
#'
#' ## setting options
#' optList <- makeOptionsList(includeFlag=TRUE)
#' mkdCodeChunkSt(rd, optionList=optList)
#' ## just leaving empty
#' mkdCodeChunkEnd(rd)
#'
setMethod(f="mkdCodeChunkSt", signature="easyreporting", 
    definition=function(object, optionList=getOptionsList(object),
                        sourceFilesList=NULL, isComplete=FALSE)
    {
        self.message <- paste0("```{r eval=", optionList$evalFlag,
                                ", echo=", optionList$echoFlag,
                                ", warning=", optionList$warningFlag,
                                ", message=", optionList$showMessages,
                                ", include=", optionList$includeFlag,
                                ", cache=", optionList$cacheFlag,
                                "}\n")
        base::write(self.message,
                file=getReportFilename(object),
                ncolumns=if(is.character(self.message)) 1 else 5,
                append=TRUE,
                sep="\n")
    
        if(!is.null(sourceFilesList))
        {
            files <- sourceFilesList
            self.message <- ""
            for(i in seq_along(files))
            {
                self.message <- paste0(self.message,
                                        "source(\"",
                                        files[[i]],
                                        "\")\n")
            }
            base::write(self.message,
                        file=getReportFilename(object),
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
)

#' mkdSourceFiles
#' @description includes a list of source files inside the rmarkdown
#' @param object an easyreporting class object
#' @param ... a list of files to source with path
#'
#' @return none
#'
#' @keywords internal
setMethod(f="mkdSourceFiles", signature="easyreporting", 
          definition=function(object, ...)
    {
        files <- list(...)
        self.message <- ""
        for(i in seq_along(files))
        {
            self.message <- paste0(self.message,
                                "source(\"",
                                file.path(files[[i]]),
                                "\")\n")
        }
        base::write(self.message,
            file=getReportFilename(object),
            ncolumns=if(is.character(self.message)) 1 else 5,
            append=TRUE,
            sep="\n")
    }
)


#' mkdCodeChunkEnd
#' @description it creates a code chunk end. Always use it after a
#' mkdCodeChunkSt()
#' @param object an easyreporting class object
#' @return none
#'
#' @export
#'
#' @examples
#'  rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdCodeChunkSt(rd)
#' ## just leaving empty
#' mkdCodeChunkEnd(rd)
setMethod(f="mkdCodeChunkEnd", signature="easyreporting", 
          definition=function(object)
    {
        self.message <- paste0("```\n")
        base::write(self.message,
            file=getReportFilename(object),
            ncolumns=if(is.character(self.message)) 1 else 5,
            append=TRUE,
            sep="\n")
    }
)

#' mkdCodeChunkComplete
#' @description it creates a complete code chunk.
#' @param object an easyreporting class object
#' @param message a string containing a function call or the entire code chunk
#' to trace.
#' @param optionList a list of options.
#' @param sourceFilesList a list of files to source.
#'
#' @return none
#' @export
#'
#' @examples
#'  rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdCodeChunkComplete(rd, message="a <- 1\nb <- 2\nc <- a+b\n print(c)")
setMethod(f="mkdCodeChunkComplete", signature="easyreporting", 
    definition=function(object, message,
                optionList=getOptionsList(object),
                sourceFilesList=NULL)
    {
        mkdCodeChunkSt(object, optionList=optionList,
                        sourceFilesList=sourceFilesList,
                        isComplete=TRUE)
        mkdGeneralMsg(object, message)
        mkdCodeChunkEnd(object)
    }
)

#' mkdCodeChunkCommented
#' @description it creates a complete code chunk, adding a natural language
#' comment before of it.
#' @param object an easyreporting class object
#' @param commentMsg a string with the natural language comment for the chunk.
#' @param codeMsg a string within the code.
#' @param optionList a list of options (default is the class options).
#' @param sourceFilesList a optional list of files to source inside the chunk.
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)
#' mkdCodeChunkCommented(rd,
#'                 commentMsg="This is the comment of the following code chunk",
#'                 codeMsg="a <- 1\nb <- 2\n(c <- a+b)\n", optionList=optList,
#'                 sourceFilesList=NULL)
#'
setMethod(f="mkdCodeChunkCommented", signature="easyreporting", 
          definition=function(object, commentMsg=NULL, codeMsg,
                                optionList=getOptionsList(object),
                                sourceFilesList=NULL)
    {
        if(!is.null(commentMsg))
        {
            mkdGeneralMsg(object, commentMsg)
        }
        mkdCodeChunkComplete(object, message=codeMsg, optionList=optionList,
                                sourceFilesList=sourceFilesList)
    }
)


#' mkdCodeChunkTitledCommented
#' @description it creates a complete code chunk, adding a natural language
#' comment before of it.
#' @param object an easyreporting class object
#' @param title the title to assign to the code chunk section
#' @param level the level of the title (default is 1)
#' @param commentMsg a string with the natural language comment for the chunk.
#' @param codeMsg a string within the code.
#' @param optionList a list of options (default is the class options).
#' @param sourceFilesList a optional list of files to source inside the chunk.
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)
#' mkdCodeChunkTitledCommented(rd, title="Title Example", level=1,
#'                 commentMsg="This is the comment of the following code chunk",
#'                 codeMsg="a <- 1\nb <- 2\n(c <- a+b)\n", optionList=optList,
#'                 sourceFilesList=NULL)
#'
setMethod(f="mkdCodeChunkTitledCommented", signature="easyreporting", 
          definition=function(object, title=NULL, level=1, 
                                commentMsg=NULL, codeMsg,
                                optionList=getOptionsList(object),
                                sourceFilesList=NULL)
        {
                mkdTitle(object, title=title, level=level)
                mkdCodeChunkCommented(object, commentMsg=commentMsg, 
                                    codeMsg=codeMsg,
                                    optionList=optionList,
                                    sourceFilesList=sourceFilesList)
        }
)


