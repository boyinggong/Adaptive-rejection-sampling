#' Generate a time series of fractional Brownian motion.
#'
#' This function generatea a time series of one dimension fractional Brownian motion.
#' adapted from http://www.mathworks.com.au/matlabcentral/fileexchange/38935-fractional-brownian-motion-generator .
#'
#' @param hurst the hurst index, with the default value 0.71
#' @param n the number of points between 0 and 1 that will be generated, with the default value 100
#' @export
#' @examples
#' fbm()
#' plot(fbm())
#' d <- fbm(hurst=0.2, n=1000)
#' plot(d)

sk <- function(x, xi, yi, k, my_domains) {
    n <- length(my_domains)-1
    integrals <- ((exp(yi + (k * (my_domains[2:(n+1)] - xi)))
    - exp(yi + (k * (my_domains[1:n] - xi)))) / k)
    return(integrals)
}