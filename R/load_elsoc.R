#' load_elsoc
#'
#' Loads the elsoc dataset into the workspace
#'
#' @param url URL specifying directory of database
#' @param format string specifying format of data to load
#'
#' @return loads the elsoc dataset into the workspace
#' @export
#'
#' @examples
#'
#' load_elsoc()
#'
load_elsoc <- function(url = NULL, format = 'long') {

    # if (is.null(url)) {
    #     url <- "https://dataverse.harvard.edu/api/access/datafile/4606527"
    # }

    if (format == 'long') {
        url <- 'https://drive.google.com/uc?export=download&id=1re1UkZfZ3foY8A7LlbmGGJ6qkCkVtaoM'
        message("Loading beta version of ELSOC 2016-2021 in Long format")
    } else {
        url <- 'https://drive.google.com/uc?export=download&id=1PLaQIRQEu6EdICzuDusoIUREEFIqoIrX'
        message("Loading beta version of ELSOC 2016-2021 in Wide format")
    }

    suppressWarnings(load(url(url), envir = globalenv()))
}
