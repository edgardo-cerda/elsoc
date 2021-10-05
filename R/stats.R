#' stats
#'
#' Calculates stats (mean, median, total and quantiles) and its variance from ELSOC considering complex survey design
#'
#' @param .data Data frame or tbl_svy survey object object
#' @param x Variable or variable name to calculate estimates
#' @param by Vector of variables to group estimates
#' @param stat Type of estimate to calculate. One of: 'mean', 'median', 'total' or 'quantile'
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv'). vartype = NULL for no variability
#' @param na.rmA logical value to indicate whether missing values of x should be dropped
#'
#' @export
#'
#' @examples
#' elsoc_example %>% stat(m0_sexo, by = ola, stat = 'mean')
#' elsoc_example %>% stat(c01, by = c(ola, m0_sexo), stat = 'median')
#'
stats <- function(.data, x, by = NULL, stat = c('mean', 'median', 'total', 'quantile'),
                 vartype = c('se', 'ci', 'var', 'cv'), quantiles = NULL, na.rm = FALSE) {
    stopifnot(!missing(x))
    if (!is.null(vartype)) {
        vartype <- if (missing(vartype)) 'se'
        else match.arg(vartype, choices = c('se', 'ci', 'var', 'cv'), several.ok = TRUE)
    }
    stat <- if (missing(stat)) 'mean'
    else match.arg(stat, choices = c('mean', 'median', 'total', 'quantile'), several.ok = FALSE)
    if (stat == 'mean') fun <- srvyr::survey_mean
    else if (stat == 'median') fun <- srvyr::survey_median
    else if (stat == 'total') fun <- srvyr::survey_total
    else if (stat == 'quantile') fun <- srvyr::survey_quantile

    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }
    if (stat != 'quantile') {
        estimates <- survey_design %>%
            dplyr::group_by(dplyr::across(!!rlang::enexpr(by))) %>%
            srvyr::summarise(stat = fun(x = !!rlang::enexpr(x),
                                        vartype = vartype,
                                        na.rm = na.rm)) %>%
            ungroup()
    } else {
        estimates <- survey_design %>%
            dplyr::group_by(dplyr::across(!!rlang::enexpr(by))) %>%
            srvyr::summarise(stat = fun(x = !!rlang::enexpr(x),
                                        quantiles = quantiles,
                                        vartype = vartype,
                                        na.rm = TRUE)) %>%
            ungroup()
    }
    return(estimates)
}

