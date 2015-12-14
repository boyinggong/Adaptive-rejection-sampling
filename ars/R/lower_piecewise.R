#' Piecewise lower hull
#'
#' This function returns the piecewise linear lower hull formed from the chords between adjacent abscissae in Tk
#'
#' @param x
#' @param x_values sorted points in T, which contains k abscissae in the domain D
#' @param y_values evaluated h at x_values, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param domains domain D, which contains the lower bound and the upper bound
#' @examples
#' lower_piecewise(x, x_values, y_values, domains)
#' lower_piecewise

lower_piecewise <- function(x, x_values, y_values, domains) {
    if (length(x_values) != length(y_values)) stop("inputs should have the same length")
    if ((length(x_values) <=1) | (length(y_values) <=1)) stop("input should have a length at least 2")
    
    ux <- ((x < x_values[1]) | (x > x_values[length(x_values)])) * -100
    n <- length(x_values)-1
    u <- ((x > x_values[1:n]) & (x <= x_values[2:(n+1)])) *
    ((x_values[2:(n+1)] - x) * y_values[1:n] +
    (x - x_values[1:n]) * y_values[2:(n+1)]) /
    (x_values[2:(n+1)] - x_values[1:n])
    return(ux+sum(u))
}