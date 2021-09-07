#' freq
#'
#' Function to create a frecuency table
#' @param .data
#' @param var = NULL
#' @param by
#' @param condition
#' @param vartype
#'
#' @return
#' @export
#'
#' @examples elsoc_long_2016_2019 %>% freq(by = c(m0_sexo, ola))
freq <- function(.data, var = NULL, by, condition, vartype = 'se') {

    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        design <- design_elsoc(.data)
    } else {
        design <- .data
    }

    # If var is defined, it's added to the 'by' variables:
    if (is.null(var)) {
        groups <- rlang::expr(!!rlang::enexpr(by))
    } else {
        groups <- rlang::expr(c(!!rlang::enexpr(by), !!rlang::enexpr(var)))
    }

    estimates <- design %>%
        dplyr::group_by(dplyr::across(!!groups)) %>%
        srvyr::summarise(prop = srvyr::survey_mean(!!enexpr(condition),
                                            proportion = TRUE,
                                            vartype = vartype))
    return(estimates)
}
