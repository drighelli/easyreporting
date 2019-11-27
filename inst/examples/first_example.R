
# library(easyreporting)
#devtools::load_all()
## creating report file with default options on global document
rd <- easyreporting(filenamepath="./project_report", title="example_report",
                        author=c("Dario Righelli"))

mkdTitle(rd, "First Level Title")

mkdGeneralMsg(rd, "Here I'm writing a simple paragraph useful to describe my code chunk")

## leaving the default options to the code chunk
mkdCodeChunkSt(rd)
## adding a variable assignement
variable <- 1
mkdVariableAssignment(rd, "variable", "variable", show=TRUE)
mkdCodeChunkEnd(rd)

mkdTitle(rd, "Second Level Title", level=2)
## or i can create my own options for the chunk
optList <- makeOptionsList(includeFlag=TRUE)
mkdCodeChunkSt(rd, optionsList=optList)
mkdCodeChunkEnd(rd)

## moreover I can add a list of files to source in che code chunk
mkdCodeChunkSt(rd, optionsList=optList, source.files.list=
                      c("R/old/cachingFunctions.R",
                      "R/old/cachingFunctions.R"))
mkdCodeChunkEnd(rd)


mkdCodeChunkComplete(rd, message="a <- 1\nb <- 2\nc <- a+b\n print(c)")


## otherwhise I can make a direct call with all the code chunk and the comment in one call
optList <- makeOptionsList(includeFlag=TRUE, cacheFlag=TRUE)

mkdCodeChunkCommented(rd, 
                commentMsg="This is the comment of the following code chunk",
                codeMsg="a <- 1\nb <- 2\n(c <- a+b)\n",
                optionsList=optList,
                sourceFilesList=NULL)

## finally I can directly compile my report
compile(rd)

