
#' is_nsnr
#'
#' Function to test if multiple variables contain the values associated with Doesn't know or Doesn't answer (default = c(-888, -999)).
#' Useful for testing multiple variables in shorter code
#'
#' @param ... variables to test
#' @param values Values to test if are contained in variables
#'
#' @export
#'
#' @examples
#' elsoc_example %>% dplyr::filter(have.value(c01, value = c(-777, -888, -999)))
#'
is_nsnr <- function(..., values = c(-666, -777, -888, -999)){
    purrr::reduce(purrr::map(.x = list(...), .f = ~{. %in% values}), `|`)
}

