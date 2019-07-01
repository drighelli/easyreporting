
# library(easyreporting)
devtools::load_all()
## creating report file with default options on global document
rd <- easyreporting$new(filenamepath="./project_report", title="example_report",
                        author=c("Dario Righelli"))

rd$mkdTitle("First Level Title")

rd$mkdGeneralMsg("Here I'm writing a simple paragraph useful to describe my code chunk")

## leaving the default options to the code chunk
rd$mkdCodeChunkSt()
## adding a variable assignement
variable <- 1
rd$mkdVariableAssignment("variable", "variable", show=TRUE)
rd$mkdCodeChunkEnd()

rd$mkdTitle("Second Level Title", level=2)
## or i can create my own options for the chunk
optList <- makeOptionsList(includeFlag=TRUE)
rd$mkdCodeChunkSt(optionsList=optList)
rd$mkdCodeChunkEnd()

## moreover I can add a list of files to source in che code chunk
rd$mkdCodeChunkSt(optionsList=optList, source.files.list=
                      c("R/cachingFunctions.R",
                      "R/cachingFunctions.R"))
rd$mkdCodeChunkEnd()


rd$mkdCodeChunkComplete(message="a <- 1\nb <- 2\nc <- a+b\n print(c)")


## otherwhise I can make a direct call with all the code chunk and the comment in one call
optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)

rd$mkdCodeChunkCommented(commentMsg="This is the comment of the following code chunk",
                    message="a <- 1\nb <- 2\n(c <- a+b)\n",
                    optionsList=optList,
                    source.files.list=NULL)

## finally I can directly compile my report
rd$compile()

