#' survey_design_elsoc
#'
#' survey_design_elsoc can be used to create a srvyr's tbl_svy object using design information from ELSOC
#'
#' @param .data data frame (ELSOC) in long format.
#' @param ids Variable or variable name in .data especifying cluster ids.
#' @param strata Variable or variable name in .data especifying strata.
#' @param weights Variable or variable name in .data especifying weights. Missing values are droped.
#' @param nest if TRUE, relabel cluster ids to enforce nesting within strata
#'
#' @export
#'
#' @examples
#' elsoc_example %>% survey_design_elsoc()
#'
survey_design_elsoc <- function(.data, ids = 'segmento_disenno', strata = 'estrato_disenno', weights = 'ponderador02', nest = TRUE) {
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

