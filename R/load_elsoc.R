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
        url <- 'https://www.dropbox.com/s/ittmfbk32n8z8eu/ELSOC_Long_2016_2021_v1.00_R.RData?dl=1'
    } else {
        url <- 'https://www.dropbox.com/s/mfsvicoyes62qk4/ELSOC_Wide_2016_2021_v1.00_R.RData?dl=1'
    }

    load(url(url), envir = globalenv())
}


