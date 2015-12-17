#' Generate random numbers using adaptive rejection sampling
#'
#' This function generate random numbers from given distribution using adaptive rejection sampling method.
#'
#' @param fun the probability density distribution to sample from
#' @param my_total_range support of the probability density distribution
#' @param n number of observations
#' @param MAX_HOLDING maximum number of points we can include in the hull.
#' @param nGroup number of samples generate at once from the upper hull
#' @param ... other values passed to fun
#' @export 
#' @examples
#' x = ars(dnorm, c(-Inf,Inf), n = 100)
#' x = ars(dnorm, c(-Inf,Inf), n = 100, mean = 2, sd = 4)
#' x = ars(dchisq, c(1,Inf), n = 1000, df = 3)


ars <- function(fun, my_total_range, n, MAX_HOLDING = 1000, nGroup = 1, ...) {
    
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
        
        gen_res <- sapply(rep(1, nGroup) , function(x){gen_xt(cs_normalized_integral_values, integral_values,
                          my_points, my_values, my_slopes, domains)})
        xt <- gen_res[1, ]
        fx <- gen_res[2, ]
        ux <- gen_res[3, ]
        
        if (!all(fx - ux > -1e-9)){
          stop("Please check the log-concavity of probability density function")
        } 
        
        # The selection criterea random variable (last random variable)
        w <- runif(min = 0, max = 1, n = nGroup)
        
        select_n <- which(w - (exp(ux - fx)) > 0)
        
        # The two checks asked about in the paper
        if (length(select_n) == 0){
            sampled_values[total_chosen:(total_chosen + nGroup - 1)] <- xt
            total_chosen <- total_chosen + nGroup
        }else{
          # Otherwise, maybe select or update the hull
          if ((length(my_points) < MAX_HOLDING) & 
              (xt[select_n[1]] > my_total_range[1] + 1e-3) &
              (xt[select_n[1]] < my_total_range[2] - 1e-3)){
            my_points <- sort(c(my_points, xt[select_n[1]]))
            my_slopes <- dh(my_points,g)
            my_values <- h(my_points)
            domains <- sort(c(total_range, get_zj(x_values = my_points,
                                                  y_values = my_values, 
                                                  slopes = my_slopes)))
          }
          if (w[select_n[1]] - (exp(h(xt[select_n[1]]) - fx[select_n[1]])) <= 0){
            sampled_values[total_chosen:(total_chosen+select_n[1]-1)] <- xt[1:select_n[1]]
            total_chosen <- total_chosen+select_n[1]
          }else if(select_n[1] > 1){
            sampled_values[total_chosen:(total_chosen+select_n[1]-2)] <- xt[1:select_n[1]-1]
            total_chosen <- total_chosen + select_n[1] - 1         
          }
        }
    }
    
    return(sampled_values)
}