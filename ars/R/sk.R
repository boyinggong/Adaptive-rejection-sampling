#' Piecewise integrals
#'
#' This function calculate the piecewise integral of the upper hull function.
#'
#' @param xj sorted points in T
#' @param hj log density evaluated at point xj
#' @param dhj slopes of piecewise upper hull
#' @param my_domains intersection points of upper hull
#' @examples
#' sk(xj, hj, dhj, my_domains)
#' sk(c(-1/2, 1/2), c(-1/2, 1/2), c(-1, 1), c(-1, 0,  1))

sk <- function(xj, hj, dhj, my_domains) {
    n <- length(my_domains)-1
    integrals <- ((exp(hj + (dhj * (my_domains[2:(n+1)] - xj)))
    - exp(hj + (dhj * (my_domains[1:n] - xj)))) / dhj)
    return(integrals)
}