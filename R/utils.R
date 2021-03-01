
.parseAuthors <- function(author)
{
    # gets a list of person and returns a formatted string 
    authors.list <- lapply(author, function(a)
    {
        if(all(is.null(a$given), is.null(a$family))) stop("Please provide the Authors given and family name!")
        a.str <- "\n      - name: \""
        a.str <- paste(a.str, a$given, sep=" ")
        a.str <- paste(a.str, a$family,  sep=" ")
        ifelse(!is.null(a$email), a.str <- paste(a.str, a$email, "\"\n", sep=" " ), a.str <- paste0(a.str, "\"\n"))
        if(!is.na(a$comment["ORCID"])) a.str <- paste0(a.str, "        orcid_id: ", a$comment["ORCID"], "\n")
        if(!is.na(a$comment["affiliation"])) a.str <- paste0(a.str, "        affiliation: ", a$comment["affiliation"], "\n")
        if(!is.na(a$comment["affiliation_url"])) a.str <- paste0(a.str, "        affiliation_url: ", a$comment["affiliation_url"], "\n")
        if(!is.na(a$comment["url"])) a.str <- paste0(a.str, "        url: ", a$comment["url"], "\n")
        return(a.str)
    })
    return(paste(authors.list, collapse=""))
}

#' @importFrom methods is
.parseCode <- function(code)
{
    if ( is(code, "list") )
    {
        strlist <- lapply(code, function(c) deparse(c))
        str <- paste(unlist(strlist), collapse="\n")
    } else if ( is(code, "<-") ) { # not working?
        print("Single")
        str <- deparse(c)
    } else {
        str <- code
    }
    return(code)
}
