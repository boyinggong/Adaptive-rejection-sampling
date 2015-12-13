#' Derivative of logarithm of functions
#'
#' This function calculate the derivative of h(x), where h(x) is the log of functions
#'
#' @param xi sorted points in T, which contains k abscissae in the domain D
#' @param g the density function used to perform the rejection sampling, where g(x) = cf(x). f(x) is the density that we actually want to sample points from
#' @examples
#' dh(xi,g,leftb,rightb)
#' dh(c(1,2,3),dnorm,-4,4)

dh <- function(xi,g) {
    xi <- as.numeric(xi)
    return(mosaic::D(log(g(x)) ~ x)(xi))
}