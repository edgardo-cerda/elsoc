freq <- function(.data, var = NULL, by, condition, vartype = 'se') {

    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        design <- design_elsoc(.data)
    } else {
        design <- .data
    }

    # If var is defined, it's added to the 'by' variables:
    if (is.null(var)) {
        print(1)
        groups <- rlang::expr(!!rlang::enexpr(by))
    } else {
        print(2)
        groups <- rlang::expr(c(!!rlang::enexpr(by), !!enexpr(var)))
    }

    estimates <- design %>%
        dplyr::group_by(dplyr::across(!!groups)) %>%
        srvyr::summarise(prop = srvyr::survey_mean(!!enexpr(condition),
                                            proportion = TRUE,
                                            vartype = vartype))
    return(estimates)
}
