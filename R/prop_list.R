#' prop_list
#'
#' Calculates proportions and its variance of a list of variables or conditions
#' from ELSOC, considering complex survey design
#'
#' @param .data Data frame or tbl_svy survey object object
#' @param x list of variables, variable names or logical vectors to calculate proportions
#' @param by Vector of variables to group estimates
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv'). vartype = NULL for no variability
#' @param na.rm A logical value to indicate whether missing values of x should be dropped
#' @param ... Other options are pass down to srvyr::survey_mean call
#'
#' @export
#'
#' @examples
#'
prop_list <- function(.data, ..., by = NULL, vartype = 'se', na.rm = FALSE) {

        # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }

    vars <- rlang::enexprs(...)
    estimates <- survey_design %>%
        purrr::map_df(.x = vars, .data = ., by = !!rlang::enexpr(by),
                      .f = prop,
                      na.rm = na.rm, vartype = vartype,
                      name.format = 2)
    return(estimates)
}


