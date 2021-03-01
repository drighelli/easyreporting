#' initReportFilename
#' @description internal funtion usefull to init the report for the first time.
#'
#' @param object an easyreporting class object 
#' @param filenamepath the name of the report with or without the path
#' @param mainTitle the title of the report
#' @param author the name of the report author as a \link[link]{person} format 
#' (where expected \code{comment} named fields are \code{ORCID}, \code{url}, 
#' \code{affiliation}, \code{affiliation_url}).
#' In case author is a \code{character}, it will be reported as it is.
#' @param documentType type of report final document, if author(s) is a \code{person} 
#' it will be \code{distill::distill_article} (\code{rmarkdown::html_document} default)
#' @param bibfile a bibfile for bibliography.
#'
#' @seealso \link[utils]{person}
#' @keywords internal
#' @importFrom tools file_ext
#'
#' @return an easyreporting class object 
setMethod(f="initReportFilename", signature="easyreporting",
        definition=function(object, 
                filenamepath=NULL, title=NULL,
                author=NULL, optionList=NULL,
                bibfile="", documentType="rmarkdown::html_document")
{
    if(is.null(filenamepath)) stop("Report file name is NULL!")
    if(is.null(title)) stop("Please provide a title for the document!")
    
    if(!file.exists(filenamepath))
    {
        if(tools::file_ext(filenamepath) != "Rmd")
        {
            filenamepath <- paste0(filenamepath, ".Rmd")
        }
        file.create(filenamepath)
    } else {
        warning("File ", filenamepath, " already exists!\n Using old one...")
    }

    object@filenamePath <- filenamepath

    if( is(author, "person") ) 
    {
        object@author <- .parseAuthors(author)
        require(distill)
        object@documentType="distill::distill_article"
    } else {
        object@author <- paste0("\"", author, "\"")
        object@documentType=documentType
    }
    # object@author <- author
    object@title <- title
    object@bibfile <- bibfile
    ## do not indent!
    header <- paste0(
        "---
    title: \"", object@title, "\"
    author: ", object@author, "
    date: \"`r Sys.Date()`\"")
    if(object@bibfile != "") header <- paste0(header, "bibfile: \"", object@bibfile, "\"")
    header <- paste0(header, "\n    output: ", object@documentType, "\n---\n")
    
    base::write(header, file=filenamepath,
                append=TRUE, sep="\n")
    object <- mkdSetGlobalOpts(object=object, optionList=optionList)

    
    return(object)
})


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
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#'
#' mkdTitle(rd, "First Level Title")
#' mkdTitle(rd, "Sub-Title", level=2)
#' }
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
            title,
            "\n"
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
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdGeneralMsg(rd, "Writing a paragraph to describe my code chunk")
#' }
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
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdGeneralTitledMsg(rd, title="Generic SubTitle for this message", level=2,
#'         message="Writing a paragraph to describe my code chunk")
#' }
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

#' getReportFilename
#' @description returns the report filename with path
#' @param object an easyreporting class object
#' @return a string of report file name with path
#' @export
#'
#' @examples
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' (rep <- getReportFilename(rd))
#' }
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
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report", title="example_report",
#'                         author=c("It's me"))
#' compile(rd)
#' }
setMethod(f="compile", signature="easyreporting",
    definition=function(object)
    {
        mkdCodeChunkTitledCommented(object=object, title="Session Info", 
            code="sessionInfo()", optionList=makeOptionsList(tidyFlag=TRUE))
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
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' ## leaving the default options to the code chunk
#' mkdCodeChunkSt(rd)
#' ## adding a variable assignement
#' variable <- 1
#' mkdVariableAssignment(rd, "variable", "variable", show=TRUE)
#' mkdCodeChunkEnd(rd)
#' }
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
#' \dontrun{
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
#' }
setMethod(f="mkdCodeChunkSt", signature="easyreporting", 
    definition=function(object, optionList=getOptionsList(object),
                        sourceFilesList=NULL, isComplete=FALSE)
    {
        self.message <- paste0("```{r eval=", optionList$eval,
                                ", echo=", optionList$echo,
                                ", warning=", optionList$warning,
                                ", message=", optionList$showMessages,
                                ", include=", optionList$include,
                                ", cache=", optionList$cache,
                                ", collapse=", object@optionList$collapse,
                                ", purl=", object@optionList$purl,
                                ", error=", object@optionList$error,
                                ", message=", object@optionList$message,
                                ", highlight=", object@optionList$highlight,
                                ", prompt=", object@optionList$prompt,
                                ", strip.white=", object@optionList$strip.white,
                                ", tidy=", object@optionList$tidy,
                                "}\n")
        base::write(self.message,
                file=getReportFilename(object),
                ncolumns=if(is.character(self.message)) 1 else 5,
                append=TRUE,
                sep="\n")
    
        if(!is.null(sourceFilesList))
        {
            destination <- gsub(basename(object@filenamePath), "", object@filenamePath)
            files <- lapply(sourceFilesList, function(file)
            {
                message("Copying ", file, " to ", file.path(destination, basename(file)))
                file.copy(from=file, to=file.path(destination, basename(file)))
                return(basename(file))
            })
            
            
            # files <- sourceFilesList
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
#' \dontrun{
#'  rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdCodeChunkSt(rd)
#' ## just leaving empty
#' mkdCodeChunkEnd(rd)
#' }
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



#' setBibliography
#' @description add a bibfile name to the object that will be reflected into the
#' report as a bibliography section
#' @param object an easyreporting class object
#' @param bibfile a string with the name of the bib file
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' # TBD
#' }
setMethod(f="setBibliography", signature="easyreporting", 
        definition=function(object, bibfile=NULL)
{
    if(!is.null(bibfile)) object@bibfile <- bibfile
})



#' getBibliography
#' @description returns the bibfile name attached to the object
#' @param object an easyreporting class object
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' # TBD
#' }
setMethod(f="getBibliography", signature="easyreporting", 
    definition=function(object)
{
      return(object@bibfile)
})

