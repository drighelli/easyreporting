#' initReportFilename-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @keywords internal
setGeneric (
    name="initReportFilename",
    def=function(object, ...){standardGeneric("initReportFilename")}
)
#' mkdSetGlobalOpts-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @keywords internal
setGeneric (
    name="mkdSetGlobalOpts",
    def=function(object, ...){standardGeneric("mkdSetGlobalOpts")}
)
#' mkdTitle-generic
#'
#' @param object a general object 
#' @param ... whatever
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
setGeneric (
    name="mkdTitle",
    def=function(object, ...){standardGeneric("mkdTitle")}
)
#' getReportFilename-generic
#'
#' @param object a general object 
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' (rep <- getReportFilename(rd))
setGeneric (
    name="getReportFilename",
    def=function(object){standardGeneric("getReportFilename")}
)
#' mkdGeneralMsg-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdGeneralMsg(rd, "Writing a paragraph to describe my code chunk")
setGeneric (
    name="mkdGeneralMsg",
    def=function(object, ...){standardGeneric("mkdGeneralMsg")}
)
#' setOptionsList-generic
#'
#' @param object a general object 
#' @param ... whatever
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
setGeneric (
    name="setOptionsList",
    def=function(object, ...){standardGeneric("setOptionsList")}
)
#' getOptionsList-generic
#'
#' @param object a general object 
#'
#' @return none
#' @export
#'
#' @examples
#' rd <- easyreporting(filenamePath="./project_report", title="example_report",
#'                         author=c("It's me"))
#' optList <- getOptionsList(rd)
#'
setGeneric (
    name="getOptionsList",
    def=function(object){standardGeneric("getOptionsList")}
)
#' compile-generic
#'
#' @param object a general object 
#'
#' @return none
#' @export
#' 
#' @examples
#' rd <- easyreporting(filenamePath="./project_report", title="example_report",
#'                         author=c("It's me"))
#' compile(rd)
#' 
setGeneric (
    name="compile",
    def=function(object){standardGeneric("compile")}
)
#' mkdVariableAssignment-generic
#'
#' @param object a general object 
#' @param ... whatever
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
setGeneric (
    name="mkdVariableAssignment",
    def=function(object, ...){standardGeneric("mkdVariableAssignment")}
)
#' mkdCodeChunkSt-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @keywords internal
setGeneric (
    name="mkdCodeChunkSt",
    def=function(object, ...){standardGeneric("mkdCodeChunkSt")}
)
#' mkdSourceFiles-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @keywords internal
setGeneric (
    name="mkdSourceFiles",
    def=function(object, ...){standardGeneric("mkdSourceFiles")}
)
#' mkdCodeChunkEnd-generic
#'
#' @param object a general object 
#'
#' @return none
#' @keywords internal
setGeneric (
    name="mkdCodeChunkEnd",
    def=function(object){standardGeneric("mkdCodeChunkEnd")}
)
#' mkdCodeChunkComplete-generic
#'
#' @param object a general object 
#' @param ... whatever
#'
#' @return none
#' @export
#'
#' @examples
#'  rd <- easyreporting(filenamePath="./project_report",
#'                         title="example_report", author=c("It's me"))
#' mkdCodeChunkComplete(rd, message="a <- 1\nb <- 2\nc <- a+b\n print(c)")
setGeneric (
    name="mkdCodeChunkComplete",
    def=function(object, ...){standardGeneric("mkdCodeChunkComplete")}
)
#' mkdCodeChunkCommented-generic
#'
#' @param object a general object 
#' @param ... whatever
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
setGeneric (
    name="mkdCodeChunkCommented",
    def=function(object, ...){standardGeneric("mkdCodeChunkCommented")}
)


