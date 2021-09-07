#' freq
#'
#' Calculates proportions and its variance from ELSOC considering complex survey design
#'
#' @param .data data frame (ELSOC) in long format
#' @param x variable, variable name or logical vector to calculate proportions
#' @param by vector of variables to group estimates
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv')
#'
#' @return
#' @export
#'
#' @examples elsoc_long_2016_2019 %>% freq(by = c(m0_sexo, ola))
freq <- function(.data, x, by, condition, vartype = 'se') {

    stopifnot(!missing(x))

    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }

    if (all(as.character(rlang::enexpr(x)) %in% names(survey_design$variables))) {
        groups <- rlang::expr(c(!!rlang::enexpr(by), !!rlang::ensym(x)))
        estimates <- survey_design %>%
            dplyr::group_by(dplyr::across(!!groups)) %>%
            srvyr::summarise(prop = srvyr::survey_mean(proportion = TRUE,
                                                       vartype = vartype))
    } else {
        groups <- rlang::expr(!!rlang::enexpr(by))
        estimates <- survey_design %>%
            dplyr::group_by(dplyr::across(!!groups)) %>%
            srvyr::summarise(prop = srvyr::survey_mean(!!rlang::enexpr(x),
                                                       proportion = TRUE,
                                                       vartype = vartype))
    }
    return(estimates)
}

