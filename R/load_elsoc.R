load_elsoc <- function(url = NULL) {
    if (is.null(url)) {
        url <- "https://dataverse.harvard.edu/api/access/datafile/4606527"
    }
    load(url(url), envir = globalenv())
}
