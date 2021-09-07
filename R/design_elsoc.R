design_elsoc <- function(.data, ids = 'segmento', strata = 'estrato', weights = 'ponderador02', nest = TRUE) {

    # Identify and remove cases with missing weights:
    nas <- is.na(.data %>% select(!!rlang::ensym(weights)))
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
    print('holi')
    return(design)
}
