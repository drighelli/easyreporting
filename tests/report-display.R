library(easyreporting)

report <- easyreporting(tempfile("report"), "Example", "Author")
report <- addResource(report, "GEO", "GSE60231", "Example data")
before <- readLines(getReportFilename(report))
display <- capture.output(result <- show(report))
stopifnot(
    identical(result, report),
    any(grepl("Title: Example", display, fixed = TRUE)),
    any(grepl(getReportFilename(report), display, fixed = TRUE)),
    any(grepl("Resources: 1", display, fixed = TRUE)),
    identical(before, readLines(getReportFilename(report)))
)

path <- tempfile("person-report", fileext = ".Rmd")
attached <- search()
if (requireNamespace("distill", quietly = TRUE)) {
    report <- easyreporting(path, "Example", utils::person("Test", "Author"))
    stopifnot(report@documentType == "distill::distill_article")
} else {
    error <- tryCatch(
        easyreporting(path, "Example", utils::person("Test", "Author")),
        error = identity
    )
    stopifnot(
        inherits(error, "error"),
        grepl("Package 'distill' is required", conditionMessage(error),
              fixed = TRUE),
        !file.exists(path)
    )
}
stopifnot(identical(attached, search()))
