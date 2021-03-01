
#' mkdCodeChunkComplete
#' @description it creates a complete code chunk.
#' @param object an easyreporting class object
#' @param code a string or an expression (or a list of expressions) generated 
#' with \link[base]{quote} containing a function call or the entire code chunk to trace.
#' @param optionList a list of options.
#' @param sourceFilesList a list of files to source.
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdCodeChunkComplete(rd, code=c(quote(a <- 1), quote(b <- 2), quote(c<-  a+b), 
#' quote(print(c))))
#' mkdCodeChunkComplete(rd, code="a <- 1\n b <- 2\n c <- a+b\n print(c)")
#' }
setMethod(f="mkdCodeChunkComplete", signature="easyreporting", 
    definition=function(object, code, 
                    optionList=getOptionsList(object), sourceFilesList=NULL)
{
    mkdCodeChunkSt(object, optionList=optionList,
    sourceFilesList=sourceFilesList,
    isComplete=TRUE)
    code <- .parseCode(code)
    mkdGeneralMsg(object, code)
    mkdCodeChunkEnd(object)
})

#' mkdCodeChunkCommented
#' @description it creates a complete code chunk, adding a natural language
#' comment before of it.
#' @param object an easyreporting class object
#' @param comment a string with the natural language comment for the chunk.
#' @param code a string within the code.
#' @param optionList a list of options (default is the class options).
#' @param sourceFilesList a optional list of files to source inside the chunk.
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)
#' mkdCodeChunkCommented(rd,
#'                 comment="This is the comment of the following code chunk",
#'                 code=c(quote(a <- 1), quote(b <- 2), quote(c <- a+b)), 
#'                 optionList=optList, sourceFilesList=NULL)
#' mkdCodeChunkCommented(rd,
#'                 comment="This is the comment of the following code chunk",
#'                 code="a <- 1\n b <- 2\n(c <- a+b)\n", optionList=optList,
#'                 sourceFilesList=NULL)
#' }
setMethod(f="mkdCodeChunkCommented", signature="easyreporting", 
          definition=function(object, comment=NULL, code,
                              optionList=getOptionsList(object),
                              sourceFilesList=NULL)
          {
              if(!is.null(comment))
              {
                  mkdGeneralMsg(object, comment)
              }
              
              mkdCodeChunkComplete(object, code=code, optionList=optionList,
                                   sourceFilesList=sourceFilesList)
          }
)


#' mkdCodeChunkTitledCommented
#' @description it creates a complete code chunk, adding a natural language
#' comment before of it.
#' @param object an easyreporting class object
#' @param title the title to assign to the code chunk section
#' @param level the level of the title (default is 1)
#' @param comment a string with the natural language comment for the chunk.
#' @param code a string within the code.
#' @param optionList a list of options (default is the class options).
#' @param sourceFilesList a optional list of files to source inside the chunk.
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)
#' mkdCodeChunkTitledCommented(rd, title="Title Example", level=1,
#'                 comment="This is the comment of the following code chunk",
#'                 code="a <- 1\n b <- 2\n(c <- a+b)\n", optionList=optList,
#'                 sourceFilesList=NULL)
#' }
setMethod(f="mkdCodeChunkTitledCommented", signature="easyreporting", 
          definition=function(object, title=NULL, level=1, 
                              comment=NULL, code,
                              optionList=getOptionsList(object),
                              sourceFilesList=NULL)
          {
              mkdTitle(object, title=title, level=level)
              mkdCodeChunkCommented(object, comment=comment, 
                                    code=code,
                                    optionList=optionList,
                                    sourceFilesList=sourceFilesList)
          }
)
