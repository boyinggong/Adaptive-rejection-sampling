#' Generate random numbers using adaptive rejection sampling
#'
#' This function generate random numbers from given distribution using adaptive rejection sampling method.
#'
#' @param fun the probability density distribution to sample from
#' @param my_total_range support of the probability density distribution
#' @param n number of observations
#' @param ... other values passed to fun
#' @param MAX_HOLDING maximum number of points we can include in the hull.
#' @export 
#' @examples
#' x = ars(dnorm, c(-Inf,Inf), n = 100)
#' x = ars(dnorm, c(-Inf,Inf), n = 100, mean = 2, sd = 4)
#' x = ars(dchisq, c(1,Inf), n = 1000, df = 3)


ars <- function(fun, my_total_range, n, MAX_HOLDING = 1000, ...) {
    
    if (!is.numeric(my_total_range)) stop("Input range is not numeric")
    if (length(my_total_range)!=2) stop("Input range length error")
    if (n%%1 != 0 | n < 1) stop("Sample size should be positive integer")
    if (my_total_range[1] >= my_total_range[2]) stop("Invalid support")
    if (MAX_HOLDING < 100) stop("Please enter a bigger maximum number of points to include in the hull")
    
    g <- function(x){fun(x, ...)}
    
    sampled_values <- rep(NaN,n)
    i <- 1
    h <- function(x) { log(g(x)) }
    
    my_points <- find_init(g, my_total_range)
    my_slopes <- dh(my_points,g)
    my_values <- h(my_points)
    
    total_range <- my_total_range
    
    total_chosen <- 1
    
    domains <- sort(c(total_range, get_zj(x_values = my_points,
    y_values = my_values,
    slopes = my_slopes)))
    # Set the domains to include the bounds as well as the first intersection points
    
    while (total_chosen <= n){
      # browser()
      # print(total_chosen)
      
        # Get the integral values, then be sure to have a normalized variant
        integral_values <- sk(my_points,my_values,my_slopes,domains)
        normalized_integral_values <- integral_values / sum(integral_values)
        
        # Now get the cumsum of the normal values
        cs_normalized_integral_values <- c(cumsum(normalized_integral_values))
        
        # Use a random variable.  This will pick which k in s_k are are choosing
        # from.
        ur <- runif(min=0,max=1,n=1)
        
        # ri is "region index" -- i.e. which segment of s we are in
        ri <- min(which(cs_normalized_integral_values >= ur))
        
        # if (min(which(cs_normalized_integral_values >= ur)) == Inf) browser()
        
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
        
        if (fx - ux < -1e-9){
          stop("Please check the log-concavity of probability density function")
        } 
        
        # The selection criterea random variable (last random variable)
        w <- runif(min = 0, max = 1, n = 1)
        
        # The two checks asked about in the paper
        if (w <= (exp(ux - fx))){
            sampled_values[total_chosen] <- xt
            total_chosen <- total_chosen+1
        }else{
            # Otherwise, maybe select or update the hull
            if (w <= (exp(h(xt) - fx))){
                sampled_values[total_chosen] <- xt
                total_chosen <- total_chosen+1
                if (length(my_points) < MAX_HOLDING){
                    my_points <- sort(c(my_points, xt))
                    my_slopes <- dh(my_points,g)
                    my_values <- h(my_points)
                    domains <- sort(c(total_range, get_zj(x_values = my_points,
                                                          y_values = my_values, 
                                                          slopes = my_slopes)))
                }
            }
        }
    }
    
    return(sampled_values)
}