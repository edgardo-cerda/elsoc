#' prop
#'
#' Calculates proportions and its variance of a variable or condition from ELSOC,
#' considering complex survey design
#'
#' @param .data Data frame or tbl_svy survey object object
#' @param x Variable, variable name or logical vector to calculate proportions
#' @param by Vector of variables to group estimates
#' @param vartype Report variability as one or more of: standard error ('se', default), confidence interval ('ci'), variance ('var') or coefficient of variation ('cv'). vartype = NULL for no variability
#' @param na.rm A logical value to indicate whether missing values of x should be dropped
#' @param name.format Controls the output's name format (1 or 2). Useful for prop_list function
#'
#' @export
#'
#' @examples
#' elsoc_example %>% prop(m0_sexo, by = ola)
#' elsoc_example %>% prop(c01 %in% c(4,5), by = c(ola, m0_sexo))
#'
prop <- function(.data, x, by, vartype,
                 na.rm = FALSE, name.format = 1, ...) {
    # Detener si no se especifíca x
    stopifnot(!missing(x))

    # Se selecciona vartype entre las opciones válidas.
    # Si no se especifica, por defecto es 'se'
    if (missing(vartype)) vartype <- 'se' else
        match.arg(vartype, choices = c('se', 'ci', 'var', 'cv'), several.ok = TRUE)

    # Si .data no es un objeto de survey.design se crea con parámetros por defecto de survey_design_elsoc
    if (!any(class(.data) %in% c('survey.design', 'survey.design2'))) {
        survey_design <- survey_design_elsoc(.data)
    } else {
        survey_design <- .data
    }

    # Se hacen los cálculos. Se divide en 2, identificando cuando x hace referencia a una variable o a una condición lógica
    if (length(rlang::enexpr(x)) == 1) {

        # Se eliminan casos con NA en x
        if (na.rm) survey_design <- dplyr::filter(survey_design, !is.na(!!rlang::enexpr(x)))

        # Se define la variable auxiliar by, para hacer el llamado de srvyr::summarise
        if (missing(by)) vars <- rlang::enquo(x) else
            vars <- rlang::expr(c(!!rlang::enexpr(by), !!rlang::enexpr(x)))

        # Se estiman las proporciones
        estimates <- survey_design %>%
            dplyr::group_by(dplyr::across(!!vars)) %>%
            srvyr::summarise(prop = srvyr::survey_mean(proportion = TRUE,
                                                       vartype = vartype,
                                                       na.rm = na.rm)) %>%
            dplyr::ungroup()

        if (name.format == 2) {
            estimates <- estimates %>%
                dplyr::rename(value = !!rlang::enexpr(x)) %>%
                dplyr::mutate(name = as.character(rlang::enexpr(x))) %>%
                dplyr::relocate(name, value)
        }

    } else {
        if (na.rm) survey_design <- dplyr::filter(survey_design, !is.na(!!rlang::enexpr(x)))

        # Si no hay by, se salta el paso de group_by():
        if (missing(by)) grouped_survey <- survey_design
        else grouped_survey <- survey_design %>% dplyr::group_by(dplyr::across(!!rlang::enexpr(by)))

        estimates <- grouped_survey %>%
            srvyr::summarise(prop = srvyr::survey_mean(!!rlang::enexpr(x),
                                                       proportion = TRUE,
                                                       vartype = vartype,
                                                       na.rm = na.rm)) %>%
            dplyr::ungroup()

        if (name.format == 2) {
            estimates <- estimates %>%
                dplyr::mutate(name = rlang::expr_text(rlang::enexpr(x))) %>%
                dplyr::relocate(name)
        }

    }

    return(estimates)
}

