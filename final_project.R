# Final Project.

simulate_things <- function(g,n) {

sampled_values <- rep(NaN,n)
i <- 1

BIG_NEG_NUM <- -1e309
  
z <- seq(from=-5, to=5, by=.01)

# g is the distribution
# h = log(g)
# dh is monotonically decreasing
# abscissae - tbd...

initial_domain = c(-3,3)

library(mosaic)

h <- function(x) { return(log(g(x))) }
dh <- function(x) {
  x <- as.numeric(x)
  return(mosaic::D(log(g(x)) ~ x)(x))
}

left_point_set <- FALSE
right_point_set <-FALSE

while (left_point_set == FALSE || right_point_set == FALSE) {
  
    if (left_point_set == FALSE) {
        x1 <- runif(n=1, min = initial_domain[1], max=initial_domain[2])
        v1 <- dh(x1)
        if (v1 > 0) {
          left_point_set <- TRUE
        }
    }

    if (right_point_set == FALSE) {
        x2 <- runif(n=1, min = initial_domain[1], max=initial_domain[2])
        v2 <- dh(x2)
        if (v2 < 0) {
          right_point_set <- TRUE
        }
    }
  print(v1)
  print(v2)
}

# At this point, we have the start points:
# u, s, l are next


get_zj <- function(dh, h, xs)
{
  zj <- rep(0, length(xs)-1)
  for (i in seq(length(zj)))
  {
      zj[i] <- (h(xs[i+1]) - h(xs[i]) - 
           (xs[i+1] * dh(xs[i+1])) + 
           (xs[i] * dh(xs[i]))) /
           (dh(xs[i]) - dh(xs[i+1]))
  }
  return(zj)
}

upper_piecewise <- function(x,
                            x_intercepts,
                            h, dh, domains) {

  domains = sort(c(domains, get_zj(dh, h, x_intercepts)))
  slopes = dh(x_intercepts)
  y_intercepts = h(x_intercepts)
  
  print("Domains, slopes, y intercepts, x intercepts")
  print(domains)
  print(slopes)
  print(y_intercepts)
  print(x_intercepts)
  
  fx <- 0
  for (i in seq(length(domains)-1)) {
     fx <- fx + 
       (x > domains[i] & x <= domains[i+1]) * 
       (((x - x_intercepts[i]) * slopes[i]) + y_intercepts[i])
  }
  return(fx)
}

lower_piecewise <- function(x, domains, h) {
  ux <- (-1e100) * (x < domains[1]) +
   (-1e100) * (x > domains[length(domains)])
  for (i in seq(length(domains)-1)) {
    ux <- ux + (x > domains[i] & x <= domains[i+1]) * 
      ((domains[i+1] - x) * h(domains[i]) + 
       (x - domains[i]) * h(domains[i+1])) / 
       (domains[i+1] - domains[i])
  }
  return(ux)
}

exp_lower_piecewise <- function(x, domains,
                            h) {
  ux <- (-1e100) * (x < domains[1]) +
    (-1e100) * (x > domains[length(domains)])
  for (i in seq(length(domains)-1)) {
    ux <- ux + (x > domains[i] & x <= domains[i+1]) * 
      ((domains[i+1] - x) * h(domains[i]) + 
         (x - domains[i]) * h(domains[i+1])) / 
      (domains[i+1] - domains[i])
  }
  return(exp(ux))
}

sk <- function(x,ltr,my_h,integration_range) {
    a <- exp_lower_piecewise(x,ltr,my_h) 
    b <- integrate(exp_lower_piecewise, lower=integration_range[1], 
               upper=integration_range[2], domains=ltr,
               h=my_h)$value
    #print(a)
    return(a / b)
}

dom <- seq(from=-5, to=5, by=0.01)

x <- seq(-10, 10, 0.01)
my_points <- c(x1,x2)
# my_slopes <- c(v1,v2)
total_range <- c(-Inf, Inf)

while (is.nan(sampled_values[length(sampled_values)]))
{
fx <- upper_piecewise(x, my_points, h, dh, total_range)

# lower_total_range <- c(x1, get_zj(dh, h, my_points), x2)

ux <- lower_piecewise(x, my_points, h)

sk_vals <- sk(x,my_points,h,total_range)

x_star <- sample(sk_vals,1)
w <- runif(min=0,max=1,n=1)
#if (is.nan((lower_piecewise(x_star, lower_total_range, h) - upper_piecewise(total_range, x_star, my_slopes, my_points, log(g(my_points))))))
#{
    print(lower_piecewise(x_star, my_points, h))
    print(upper_piecewise(x_star, my_points, h, dh, total_range))
#}

if (w <= (exp(lower_piecewise(x_star, my_points, h) - upper_piecewise(x_star, my_points, h, dh, total_range))))
{
  sampled_values[i] <- x_star
  i <- i + 1
}
else {
  if (w <= (exp(h(x_star) - upper_piecewise(x_star, my_points, h, dh, total_range))))
  {
    sampled_values[i] <- x_star
    i <- i + 1    
  }
  # remake T's, sk's, l's, u's
  my_points <- sort(c(my_points, x_star))
}

# Plot
plot(x = x, y = log(dnorm(x)), type="l",ylim=c(-8,3), xlim=c(-2, 2))
points(x = x, y = fx, type="l", ylim=c(-5,3))
points(x = x, y = ux, type="l", ylim=c(-5,3))
}
return(sampled_values)
}

sv <- simulate_things(dnorm,10)
histogram(sv)