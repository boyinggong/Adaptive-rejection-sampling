#' Piecewise linear upper hull
#'
#' This function returns the piecewise linear upper hulls from the tangents to h(x) at the abscissae x_intercepts in Tk
#'
#' @param cs_normalized_integral_values normalized integral exponential upper hull
#' @param integral_values piecewise integrated upper hull function
#' @param my_points sorted points in T, which contains k abscissae in the domain D
#' @param my_values evaluated h at x_intercepts, where h = log g(x). g is the density function used to perform the rejection sampling
#' @param my_slopes slope of h evaluated at x_intercepts; they are the outputs of dh function calculated at x1 through xk
#' @param domains domain D, which contains the lower bound and the upper bound

gen_xt <- function(cs_normalized_integral_values, integral_values, 
                   my_points, my_values, my_slopes, domains){
  ur <- runif(min=0,max=1,n=1)
  
  # ri is "region index" -- i.e. which segment of s we are in
  ri <- min(which(cs_normalized_integral_values >= ur))
  
  # The the uniform random variable for the inverse CDF of s_i(x)
  ur <- runif(min=0,max=1,n=1)
  # the slope
  s <- my_slopes[ri]
  # re-normalizing factor
  nrmlztn <- 1/integral_values[ri]
  
  # This is the functional form for the inverse cdf.
  if (s == 0){
    xt <- ur * integral_values[ri] / exp(my_values[ri]) + domains[ri]
  }else{
    xt <- log(((s * ur)/(nrmlztn * exp(my_values[ri] - s * my_points[ri]))) + 
                exp(s * domains[ri])) / s
  }
  # The upper hull evaluated at xt
  fx <- upper_piecewise(x = xt,
                        x_intercepts = my_points,
                        y_values = my_values,
                        slopes = my_slopes,
                        domains = domains)
  # The lower hull evaluated at xt
  ux <- lower_piecewise(x = xt,
                        x_values = my_points,
                        y_values = my_values,
                        domains = domains)
  return(c(xt, fx, ux))
}