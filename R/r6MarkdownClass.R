
# optionsList <- list(
#     cacheFlag=TRUE,
#     evalFlag=TRUE,
#     echoFlag=TRUE,
#     warningFlag=FALSE,
#     showMessages=FALSE,
#     includeFlag=FALSE
# )

#' Title
#'
#' @param filenamepath
#' @param title
#' @param author
#'
#' @return
#' @export
#'
#' @examples

r6markdown <- R6::R6Class("r6markdown",
   public=list(
       initialize=function(filenamepath, title=NULL, author=NULL)
       {
            private$initReportFilename(filenamepath=filenamepath,
                                        mainTitle=title, author=author,
                                        optionsList=private$optionsList)
       },
       setOptionList=setOptionsList,
       getOptionList=getOptionsList,
       mkdGeneralMsg=mkdGeneralMsg,
       mkdTitle=mkdTitle,
       compile=compile,
       getReportFilename=getReportFilename
   ),
   private=list(
        ## methods
        initReportFilename=initReportFilename,
        markdownSetGlobalOpts=mkdSetGlobalOpts,

        ## attributes
        filenamePath=NULL,
        optionsList=list(
            cacheFlag=TRUE,
            evalFlag=TRUE,
            echoFlag=TRUE,
            warningFlag=FALSE,
            showMessages=FALSE,
            includeFlag=FALSE
        )
    )
)



