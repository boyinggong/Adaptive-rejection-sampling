#' Piecewise linear upper hull
#'
#' This function returns the piecewise linear upper hulls from the tangents to h(x) at the abscissae x_intercepts in Tk
#'
#' @param x the points we want to evaluate at
#' @param x_intercepts sorted points in T, which contains k abscissae in the domain D
#' @param y_values evaluated h at x_intercepts, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at x_intercepts; they are the outputs of dh function calculated at x1 through xk
#' @param domains domain D, which contains the lower bound and the upper bound

# The upper piecewise function
upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
  
  if (length(x_intercepts) != length(y_values) | 
      length(x_intercepts) != length(slopes) | 
      length(y_values) != length(slopes) |
      length(domains) - 1 != length(slopes))
    stop("inputs length inconsistent")
  if (x < domains[1] | x > domains[length(domains)])
    stop("inputs are not defined on the given domain")

    n <- length(domains)-1
    fx <- (x > domains[1:n] & x <= domains[2:(n+1)]) *
    (((x - x_intercepts[1:n]) * slopes[1:n]) + y_values[1:n])
    return(sum(fx))
}