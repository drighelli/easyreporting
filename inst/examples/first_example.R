
# library(easyreporting)
devtools::load_all()
## creating report file with default options on global document
rd <- r6markdown$new(filenamepath="./project_report", title="example_report",
                        author=c("Dario Righelli"))

rd$mkdTitle("First Level Title")

rd$mkdGeneralMsg("Here I'm writing a simple paragraph useful to describe my code chunk")

## leaving the default options to the code chunk
rd$mkdCodeChunkSt()
## adding a variable assignement
variable <- 1
rd$mkdVariableAssignment("variable", "variable")
rd$mkdCodeChunkEnd()

## or i can create my own options for the chunk
optList <- maketOptionsList(includeFlag=TRUE)
rd$mkdCodeChunkSt(optionsList=optList)
rd$mkdCodeChunkEnd()

## moreover I can add a list of files to source in che code chunk
rd$mkdCodeChunkSt(optionsList=optList, source.files.list=
                      c("R/cachingFunctions.R",
                      "R/cachingFunctions.R"))
rd$mkdCodeChunkEnd()




rd$compile()

