#' Intersection points zj
#'
#' This function returns the intersection points generated from piecewise linear upper hull
#'
#' @param x_values sorted points in T, which contains k abscissae in the domain D
#' @param y_values evaluated h at x_values, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at x_values; they are the outputs of dh function calculated at x1 through xk

# Get the intersection points (the 'z_i' defined by the paper)
get_zj <- function(x_values, y_values, slopes)
{
    n <- length(x_values)
    zj <- (y_values[2:n] - y_values[1:(n-1)] -
             (x_values[2:n] * slopes[2:n]) +
             (x_values[1:(n-1)] * slopes[1:(n-1)])) /
             (slopes[1:(n-1)] - slopes[2:n])
    eq_indx <- which(slopes[1:(n-1)] - slopes[2:n] == 0)
    if (length(eq_indx)) zj[eq_indx] = (x_values[eq_indx] + x_values[eq_indx+1])/2
    return(zj)
}
