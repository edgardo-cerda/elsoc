#' load_elsoc
#'
#' Loads the elsoc dataset into the workspace
#'
#' @param data string specifying format of data to load. Options are:
#' - 'long' (default)
#' - 'wide'
#' - 'territorial-2016'
#' - 'territorial-2017'
#' - 'territorial-2018'
#' - 'territorial-2019'
#' - 'elsoc-cit-2016'
#'
#' @param name specify the name in which the data object is stored
#'
#' @return loads the elsoc dataset into the workspace
#' @export
#'
#' @examples
#'
#' load_elsoc()
#' load_elsoc(name = 'anyname')
#' load_elsoc(data = 'wide')
#' load_elsoc(data = 'territorial-2019')
#'
load_elsoc <- function(data = 'long', name = NULL) {

    if (data == 'long') {
        if (is.null(name)) name <- 'elsoc_long_2016_2021'
        load(url('https://dataverse.harvard.edu/api/access/datafile/6160173'))
        dataobj <- elsoc_long_2016_2021
    }

    else if (data == 'wide') {
        if (is.null(name)) name <- 'elsoc_wide_2016_2021'
        load(url('https://dataverse.harvard.edu/api/access/datafile/6160174'))
        dataobj <- elsoc_wide_2016_2021
    }

    else if (data == 'territorial-2016') {
        if (is.null(name)) name <- 'elsoc_terr_2016'
        dataobj <- read.table(url('https://dataverse.harvard.edu/api/access/datafile/5216817'), sep = '\t',
                                  header = TRUE, fileEncoding = "UTF-8")
    }

    else if (data == 'territorial-2017') {
        if (is.null(name)) name <- 'elsoc_terr_2017'
        dataobj <- read.table(url('https://dataverse.harvard.edu/api/access/datafile/5216836'), sep = '\t',
                                  header = TRUE, fileEncoding = "UTF-8")
    }

    else if (data == 'territorial-2018') {
        if (is.null(name)) name <- 'elsoc_terr_2018'
        dataobj <- read.table(url('https://dataverse.harvard.edu/api/access/datafile/5216941'), sep = '\t',
                                  header = TRUE, fileEncoding = "UTF-8")
    }

    else if (data == 'territorial-2019') {
        if (is.null(name)) name <- 'elsoc_terr_2019'
        dataobj <- read.table(url('https://dataverse.harvard.edu/api/access/datafile/5216990'), sep = '\t',
                                  header = TRUE, fileEncoding = "UTF-8")
    }

    else if (data == 'elsoc-cit-2016') {
        if (is.null(name)) name <- 'elsoc_cit_2016'
        dataobj <- read.table(url('https://dataverse.harvard.edu/api/access/datafile/4892173'), sep = '\t',
                                  header = TRUE, fileEncoding = "UTF-8")
    }

    assign(name, dataobj, envir = .GlobalEnv)

}
