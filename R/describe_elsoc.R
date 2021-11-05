#' describe_elsoc
#'
#' Gives a full description of a variable, presenting variable's code, label, question,
#' module, answers values and label values, and waves and samples in which it was measured
#'
#' @param x Variable name as a string
#'
#' @export
#'
describe_elsoc <- function(x) {

    values <- values_elsoc(x)
    question <- elsoc::variable_list$question[elsoc::variable_list$variable == x]
    label <- elsoc::variable_list$label[elsoc::variable_list$variable == x]
    module <- elsoc::variable_list$module[elsoc::variable_list$variable == x]

    measures <- elsoc::variable_list[elsoc::variable_list$variable == x,
                                     c(glue::glue('ola_{c(2016:2019, 2021)}_m1'),
                                       glue::glue('ola_{c(2018:2019, 2021)}_m2'))] %>%
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = c('drop', 'wave', 'sample'),
                            names_sep = '_') %>%
        dplyr::select(-drop) %>%
        tidyr::pivot_wider(names_from = sample, values_from = value)

    description <- list(var_code = x,
                        label = label,
                        question = question,
                        module = module,
                        values = values,
                        measures = measures)
    return(description)
}
