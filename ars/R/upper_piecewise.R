#' Piecewise linear upper hull
#'
#' This function returns the piecewise linear upper hulls from the tangents to h(x) at the abscissae xj in Tk
#'
#' @param xj sorted points in T, which contains k abscissae in the domain D
#' @param hj evaluated h at xj, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param slopes slope of h evaluated at xj; it should be the output of dh function calculated at x1 through xk
#' @param leftb left bound of our domain D
#' @param rightb right bound of our domain D
#' @examples
#' upper_piecewise(x, x_intercepts, hj, slopes, leftb, rightb)
#' upper_piecewise


#upper_piecewise <- function(x, x_intercepts, hj, slopes, leftb, rightb) {
#    n <- length(domains)-1
#    fx <- (x > domains[1:n] & x <= domains[2:(n+1)]) *
#    (((x - x_intercepts[1:n]) * slopes[1:n]) + hj[1:n])
#    return(sum(fx))
#}

upper_piecewise <- function(xj, x_intercepts, hj, slopes, leftb, rightb) {
    if (xi < leftb || xi >rightb) stop("inputs are not defined on the given domain")
    
    n <- length(xj)
    zj <- (hj[2:n] - hj[1:(n-1)] - (xj[2:n]*slopes[2:n]) + (xj[1:(n-1)]*slopes[1:(n-1)])) / (slopes[1:(n-1)] - slopes[2:n])
    ## add z0 and zk to the zj's
    # z0 is the lower bound of D (-Inf if D is not bounded below)
    # zk is the upper bound of D (Inf if D is not bounded above)
    # pervious k-1 zj's, now k+1
    zj <- c(leftb,zj,rightb)
    uk <- rep(NA,n)
    uk <- hj[1:n] + (x-xj[1:n])*slopes[1:n]

    return(uk)
