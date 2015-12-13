#' Intersection points zj
#'
#' This function returns the intersection points generated from piecewise linear upper hull
#'
#' @param x_values sorted points in T, which contains k abscissae in the domain D
#' @param y_values evaluated h at x_values, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at x_values; they are the outputs of dh function calculated at x1 through xk
#' @examples
#' get_zj(x_values, y_values, slopes)
#' get_zj(c(1,2,3), c(4,8,12), c(1, 0,-1))


get_zj <- function(x_values, y_values, slopes) {
    if (length(x_values) != length(y_values) | length(x_values) != length(slopes) | length(y_values) != length(slopes)) stop("inputs should have the same length")
    if (length(x_values) <=1 | length(y_values) <=1 | length(slopes) <=1) stop("input should have a length at least 2")
    
    n <- length(x_values)
    zj <- (y_values[2:n] - y_values[1:(n-1)] - (x_values[2:n] * slopes[2:n]) + (x_values[1:(n-1)] * slopes[1:(n-1)])) / (slopes[1:(n-1)] - slopes[2:n])
    return(zj)
}
