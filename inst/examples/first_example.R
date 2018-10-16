
# library(easyreporting)
devtools::load_all()
## creating report file with default options on global document
rd <- r6markdown$new(filenamepath="./project_report", title="example_report",
                        author=c("Dario Righelli"))

rd$mkdTitle("First Level Title")


rd$compile()

