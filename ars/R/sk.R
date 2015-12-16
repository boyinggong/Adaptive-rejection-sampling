#' Piecewise integrals
#'
#' This function calculate the piecewise integral of the upper hull function.
#'
#' @param xj sorted points in T
#' @param hj log density evaluated at point xj
#' @param dhj slopes of piecewise upper hull
#' @param my_domains intersection points of upper hull

sk <- function(xj, hj, dhj, my_domains) {
    if (length(my_domains) != length(xj)+1 |length(my_domains) != length(hj)+1 |
        length(my_domains) != length(dhj)+1) stop("length inconsistent")
    if (length(xj) <= 1) stop("number of points should be greater than 2")
    
    n <- length(my_domains)-1
    integrals <- ((exp(hj + (dhj * (my_domains[2:(n+1)] - xj)))
                   - exp(hj + (dhj * (my_domains[1:n] - xj)))) / dhj)
    zero_slope_indx <- which(dhj == 0)
    if (length(zero_slope_indx)) 
      integrals[zero_slope_indx] = exp(hj[zero_slope_indx]) * (my_domains[zero_slope_indx+1] - my_domains[zero_slope_indx])
    return(integrals)
}