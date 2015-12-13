#' Intersection points zj
#'
#' This function returns the intersection points generated from piecewise linear upper hull
#'
#' @param xj sorted points in T, which contains k abscissae in the domain D
#' @param hj evaluated h at xj, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at xj; they are the outputs of dh function calculated at x1 through xk
#' @examples
#' get_zj(xj, hj, slopes)
#' get_zj(c(1,2,3), c(4,8,12), c(1, 0,-1))


get_zj <- function(xj, hj, slopes) {
    if (length(xj) != length(hj) | length(xj) != length(slopes) | length(hj) != length(slopes)) stop("inputs should have the same length")
    if (length(xj) <=1 | length(hj) <=1 | length(slopes) <=1) stop("input should have a length at least 2")
    
    n <- length(x_values)
    zj <- (y_values[2:n] - y_values[1:(n-1)] - (x_values[2:n] * slopes[2:n]) + (x_values[1:(n-1)] * slopes[1:(n-1)])) / (slopes[1:(n-1)] - slopes[2:n])
    return(zj)
}
