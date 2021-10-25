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
        url <- 'https://www.dropbox.com/s/i3y7yoljhna7jtq/ELSOC_Long_2016_2021_v1.00_R.RData?dl=1'
        message("Loading beta version of ELSOC 2016-2021 in Long format")
    } else {
        url <- 'https://www.dropbox.com/s/zbr2ls05wu3fimc/ELSOC_Wide_2016_2021_v1.00_R.RData?dl=1'
        message("Loading beta version of ELSOC 2016-2021 in Wide format")
    }

    suppressWarnings(load(url(url), envir = globalenv()))
}
