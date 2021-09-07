#' design_elsoc
#'
#' @param .data
#' @param ids = "segmento"
#' @param strata = "estrato"
#' @param weights = "ponderador02"
#' @param nest = TRUE
#'
#' @return
#' @export
#'
#' @examples elsoc_long_2016_2021 %>% design_elsoc()
design_elsoc <- function(.data, ids = 'segmento', strata = 'estrato', weights = 'ponderador02', nest = TRUE) {

    # Identify and remove cases with missing weights:
    nas <- is.na(.data %>% dplyr::select(!!rlang::ensym(weights)))
    sum_nas <- sum(nas)
    if (sum_nas>0) {
        .data <- .data[!nas, ]
        message(glue::glue("{sum_nas} cases with missing weights. Droped from survey design."))
    }

    # Create survey design object
    design <- .data %>%
        srvyr::as_survey(ids = !!rlang::ensym(ids),
                         strata = !!rlang::ensym(strata),
                         weights = !!rlang::ensym(weights), nest = nest)
    return(design)
}
