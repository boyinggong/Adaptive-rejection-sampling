#' Piecewise linear upper hull
#'
#' This function returns the piecewise linear upper hulls from the tangents to h(x) at the abscissae xj in Tk
#'
#' @param x
#' @param xj sorted points in T, which contains k abscissae in the domain D
#' @param hj evaluated h at xj, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at xj; they are the outputs of dh function calculated at x1 through xk
#' @param domains domain D, which contains the lower bound and the upper bound
#' @examples
#' upper_piecewise(x, x_intercepts, hj, slopes, leftb, rightb)
#' upper_piecewise(4,c(1,23,3),c(2,1,4),c(-1,0,10),c(-5,5))


upper_piecewise <- function(x, xj, hj, slopes, domains) {
    if (x > domains[2] | x < domains[1]) stop("inputs are not defined on the given domain")
    if (length(xj) != length(hj) | length(xj) != length(slopes) | length(hj) != length(slopes)) stop("inputs should have the same length")
    if (length(xj) <=1 | length(hj) <=1 | length(slopes) <=1) stop("input should have a length at least 2")
    n <- length(domains)-1
    fx <- (x > domains[1:n] & x <= domains[2:(n+1)]) *
    (((x - xj[1:n]) * slopes[1:n]) + hj[1:n])
    return(sum(fx))
}

