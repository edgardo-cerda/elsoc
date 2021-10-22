#' lookfor
#'
#' Function to find variable names, general concept and label based on a pattern in the ELSOC variable list. Regular expressions for the pattern is supported
#' @param pattern Character string to be looked for in the ELSOC variable list
#'
#' @export
#'
#' @examples
#'
#' lookfor('c01')
#' lookfor('cohesion')
lookfor <- function(pattern = '.') {

        pos1 <- which(grepl(pattern = pattern, x = tolower(elsoc::variable_list$variable)))
        pos2 <- which(grepl(pattern = pattern, x = tolower(elsoc::variable_list$label)))
        pos3 <- which(grepl(pattern = pattern, x = tolower(elsoc::variable_list$general_concept)))

        pos <- unique(c(pos1, pos2, pos3))
        return(elsoc::variable_list[pos, c('variable', 'label', 'general_concept')])
}
