#' prop_by_list
#'
#' Calculates proportions and its variance of a variable or condition by a list of variables
#' from ELSOC, considering complex survey design
#'
#' @param .data Data frame or tbl_svy survey object object
#' @param x Variable or logical vectors to calculate proportions
#' @param ... Variables to group estimates
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv'). vartype = NULL for no variability
#' @param na.rm A logical value to indicate whether missing values of x should be dropped
#'
#' @export
#'
#' @examples
#'
prop_list <- function(.data, x, ... , vartype = 'se', na.rm = FALSE) {

    # If .data is not a survey.design object it is created
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }

    by_vars <- rlang::enexprs(...)
    estimates <- survey_design %>%
        purrr::map_df(.x = by_vars, .f = function(x) {prop(.data = ., x = x, by = .x,
                      na.rm = na.rm, vartype = vartype, name.format = 3) } )
    return(estimates)
}


### Falta:
# hay que hacer que función prop cambie nombre de valores de by
# automáticamente cuando name.format = 3, para que se puedan juntar en un único DF
# el problema es que a diferencia de x, by puede tener más de una variable y eso complica todo


