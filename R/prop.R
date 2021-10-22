#' prop
#'
#' Calculates proportions and its variance of a variable or condition from ELSOC,
#' considering complex survey design
#'
#' @param .data Data frame or tbl_svy survey object object
#' @param x Variable, variable name or logical vector to calculate proportions
#' @param by Vector of variables to group estimates
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv'). vartype = NULL for no variability
#' @param as.na Treat values of x as NA
#' @param na.rm A logical value to indicate whether missing values of x should be dropped
#' @param ... Other options are pass down to srvyr::survey_mean call
#'
#' @export
#'
#' @examples
#' elsoc_example %>% prop(m0_sexo, by = ola)
#' elsoc_example %>% prop(c01 %in% c(4,5), by = c(ola, m0_sexo))
#'
prop <- function(.data, x, by, vartype = c('se', 'ci', 'var', 'cv'),
                 as.na = NULL, as.factor = FALSE, na.rm = FALSE, ...) {
    # Stop if x is not specified
    stopifnot(!missing(x))
    # Select vartype among valid options. Default is 'se' if not specified
    if (!is.null(vartype)) {
        vartype <- if (missing(vartype)) 'se'
        else match.arg(vartype, choices = c('se', 'ci', 'var', 'cv'), several.ok = TRUE)
    }
    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }
    # If x is not a symbol or a character, it is assumed is a logical condition:
    if (is.symbol(rlang::enexpr(x)) | is.character(rlang::enexpr(x))) {
        if (!is.null(as.na)) {
            survey_design <- dplyr::mutate(survey_design, !!rlang::enexpr(x) := ifelse(!!rlang::enexpr(x) %in% as.na, NA, !!rlang::enexpr(x)))
        }

        if (na.rm) survey_design <- dplyr::filter(survey_design, !is.na(!!rlang::enexpr(x)))

        if (missing(by)) vars <- c(rlang::enexpr(x))
        else vars <- c(rlang::enexpr(by), rlang::enexpr(x))

        estimates <- survey_design %>%
            dplyr::group_by(!!!vars) %>%
            srvyr::summarise(prop = srvyr::survey_mean(proportion = TRUE,
                                                       vartype = vartype,
                                                       na.rm = na.rm,
                                                       ...)) %>%
            dplyr::ungroup()
    } else {
        if (na.rm) survey_design <- dplyr::filter(survey_design, !is.na(!!rlang::enexpr(x)))
        estimates <- survey_design %>%
            dplyr::group_by(!!rlang::enexpr(by)) %>%
            srvyr::summarise(prop = srvyr::survey_mean(!!rlang::enexpr(x),
                                                       proportion = TRUE,
                                                       vartype = vartype,
                                                       na.rm = na.rm,
                                                       ...)) %>%
            dplyr::ungroup()
    }

    return(estimates)
}
