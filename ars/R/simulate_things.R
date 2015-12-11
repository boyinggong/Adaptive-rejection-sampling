#' Generate random numbers using adaptive rejection sampling
#'
#' This function generate random numbers from given distribution using adaptive rejection sampling method.
#'
#' @param g the probability density distribution to sample from
#' @param my_total_range support of the probability density distribution
#' @param n number of observations.
#' @export 
#' @examples
#' simulate_things(dnorm, c(-Inf,Inf), n = 10000)


simulate_things <- function(g, my_total_range, n) {
    
    MAX_HOLDING <- 1000
    
    # Fill a blank array with NaN's to replace later
    sampled_values <- rep(NaN,n)
    # Initialize the counter
    i <- 1
    
    # DEPRECATED: the least negative number
    BIG_NEG_NUM <- -1e309
    
    z <- seq(from=-5, to=5, by=.01)
    
    # This is could be set as something else, but it's the range
    # that we need to check to find our first two tangent points
    
    initial_domain = c(-3,3)
    
    # dh(u)/du at some x (= d(log(g(u)))/du at some x)
    # do people like this package?  We could use another one.
    
    h <- function(x) { log(g(x)) }
    
    # have we found our starting points?  Set to false and will become TRUE.
    left_point_set <- FALSE
    right_point_set <-FALSE
    
    # To exit the loop, we need to be on other side of the concave up/down
    while (left_point_set == FALSE || right_point_set == FALSE) {
        # left point must be concave UP
        if (left_point_set == FALSE) {
            x1 <- runif(n=1, min = initial_domain[1], max=initial_domain[2])
            v1 <- dh(x1,g)
            if (v1 > 0) {
                left_point_set <- TRUE
            }
        }
        # right point must be concave DOWN
        if (right_point_set == FALSE) {
            x2 <- runif(n=1, min = initial_domain[1], max=initial_domain[2])
            v2 <- dh(x2,g)
            if (v2 < 0) {
                right_point_set <- TRUE
            }
        }
    }
    
    # At this point, we have the start points:
    # u, s, l are next
    
    dom <- seq(from=-5, to=5, by=0.01)
    
    x <- seq(-100, 100, 0.001)
    
    my_points <- c(x1,x2)
    my_slopes <- dh(my_points,g)
    my_values <- h(my_points)
    
    total_range <- my_total_range
    
    total_chosen <- 1
    
    domains <- sort(c(total_range, get_zj(x_values = my_points,
    y_values = my_values,
    slopes = my_slopes)))
    
    # Set the domains to include the bounds as well as the first intersection points
    
    # Don't stop until we have as many samples as we required
    while (total_chosen <= n){
        # Get the integral values, then be sure to have a normalized variant
        integral_values <- sk(my_points,my_values,my_slopes,domains)
        normalized_integral_values <- integral_values / sum(integral_values)
        
        # Now get the cumsum of the normal values
        cs_normalized_integral_values <- c(cumsum(normalized_integral_values))
        total_integral_values <- integral_values
        
        # Use a random variable.  This will pick which k in s_k are are choosing
        # from.
        ur <- runif(min=0,max=1,n=1)
        
        # ri is "region index" -- i.e. which segment of s we are in
        ri <- min(which(cs_normalized_integral_values >= ur))
        
        # The the uniform random variable for the inverse CDF of s_i(x)
        ur <- runif(min=0,max=1,n=1)
        # the domain for the first part
        zi <- domains[ri]
        # the slope
        s <- my_slopes[ri]
        # re-normalizing factor
        nrmlztn <- 1/integral_values[ri]
        # our y-value to define h(x)
        y <- my_values[ri]
        # our x-position to define h(x)
        xc <- my_points[ri]
        
        # This is the functional form for the inverse cdf I got to. I will
        # send out a little latex as the seed of our paper that will explain
        # how I got here.
        xt <- log(((s * ur)/(nrmlztn * exp(y - s * xc))) + exp(s * zi)) / s
        
        # DEPRECATED: shows me what is going on
        # print("Xt and inside")
        # print(xt)
        # print(((s * ur)/(nrmlztn * exp(y - s * xc))) + exp(s * zi))
        
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