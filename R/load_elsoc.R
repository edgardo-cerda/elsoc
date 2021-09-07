#' load_elsoc
#'
#' Loads the elsoc dataset into the workspace
#'
#' @param url = "https://dataverse.harvard.edu/api/access/datafile/4606527"
#'
#' @return loads the elsoc dataset into the workspace
#' @export
#'
#' @examples load_elsoc()
load_elsoc <- function(url = NULL) {

    if (is.null(url)) {
        url <- "https://dataverse.harvard.edu/api/access/datafile/4606527"
    }
    load(url(url), envir = globalenv())
}

