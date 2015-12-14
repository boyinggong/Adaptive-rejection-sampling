#' Derivative of logarithm of functions
#'
#' This function calculate the derivative of h(x), where h(x) is the log of functions
#'
#' @param x sorted points in T, which contains k abscissae in the domain D
#' @param g the density function used to perform the rejection sampling, where g(x) = cf(x). f(x) is the density that we actually want to sample points from
#' @examples
#' dh(x,g)
#' dh(c(1,2,3),dnorm)

<<<<<<< HEAD
dh <- function(x,g) {
    x <- as.numeric(x)
    #grad <- numericDeriv(quote(log(g(x))),"x")
    #return(attr(grad,"gradient"))
    return(mosaic::D(log(g(x)) ~ x)(x))
=======
dh <- function(xi,g) {
    xi <- as.numeric(xi)
    gradMat <- attr(numericDeriv(quote(log(g(xi))),"xi"),"gradient")
    out <- diag(gradMat)
    return(out)
>>>>>>> 171c05c021c0b48e5c0c26f8a285e84c250027df
}
