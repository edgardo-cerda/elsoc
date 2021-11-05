#' values_elsoc
#'
#' Function to return variable values and value labels
#'
#' @param x Variable name as a string
#'
#' @export
#'
values_elsoc <- function(x) {

    values <- data.frame(strsplit(elsoc::variable_list$codigos_respuesta[elsoc::variable_list$variable == x], '\\|'),
                         strsplit(elsoc::variable_list$opciones_respuesta[elsoc::variable_list$variable == x], '\\|'))
    names(values) <- c('values', 'value_labels')

    return(values)

}

