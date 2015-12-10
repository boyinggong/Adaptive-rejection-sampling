library(rbenchmark)

x_values <- seq(1, 3, length.out = 1000)
y_values <- seq(2, 4, length.out = 1000)
slopes <- seq(3, 5, length.out = 1000)
x  <- rnorm(1, 5, 7)
x_intercepts <- seq(5, 7, length.out = 1000)
domains <- rnorm(1001, 5, 7)

my_points <- rnorm(1000)
my_values <- rnorm(1000)
my_slopes <- rnorm(1000)
my_domains <- rnorm(1001)

################    original function   ################

get_zj <- function(x_values, y_values, slopes)
{
  zj <- rep(0, length(x_values)-1)
  for (i in seq(length(zj)))
  {
    zj[i] <- (y_values[i+1] - y_values[i] - 
                (x_values[i+1] * slopes[i+1]) + 
                (x_values[i] * slopes[i])) /
      (slopes[i] - slopes[i+1])
  }
  return(zj)
}

################    vectorized function   ################

get_zj_new <- function(x_values, y_values, slopes)
{
  n <- length(x_values)
  zj <- (y_values[2:n] - y_values[1:(n-1)] -
           (x_values[2:n] * slopes[2:n]) +
           (x_values[1:(n-1)] * slopes[1:(n-1)])) /
    (slopes[1:(n-1)] - slopes[2:n])
  return(zj)
}

################          test           ################

benchmark(zj <- get_zj(x_values, y_values, slopes), zj_new <- get_zj_new(x_values, y_values, slopes))
all(zj == zj_new)

################    original function    ################

upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
  fx <- 0
  for (i in seq(length(domains)-1)) {
    fx <- fx + 
      (x > domains[i] & x <= domains[i+1]) * 
      (((x - x_intercepts[i]) * slopes[i]) + y_values[i])
  }
  return(fx)
}

################    vectorized function   ################

upper_piecewise_new <- function(x, x_intercepts, y_values, slopes, domains) {
  n <- length(domains)-1
  fx <- (x > domains[1:n] & x <= domains[2:(n+1)]) * 
    (((x - x_intercepts[1:n]) * slopes[1:n]) + y_values[1:n])
  return(sum(fx))
}

################          test           ################

options(digits = 22)
upper_piecewise(x, x_intercepts, y_values, slopes, domains)
upper_piecewise_new(x, x_intercepts, y_values, slopes, domains)

## test: use test_equal and allow slight numeric difference

################    original function   ################

# The exponential of the upper piecewise function
exp_upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
  fx <- 0
  for (i in seq(length(domains)-1)) {
    fx <- fx + 
      (x > domains[i] & x <= domains[i+1]) * 
      (((x - x_intercepts[i]) * slopes[i]) + y_values[i])
  }
  return(exp(fx))
}

################    vectorized function   ################

exp_upper_piecewise_new <- function(x, x_intercepts, y_values, slopes, domains) {
  return(exp(upper_piecewise_new(x, x_intercepts, y_values, slopes, domains)))
}

################          test           ################

options(digits = 22)
exp_upper_piecewise(x, x_intercepts, y_values, slopes, domains)
exp_upper_piecewise_new(x, x_intercepts, y_values, slopes, domains)

################    original function   ################

# The s upper bound function (the new pdf from which to sample)
# returns the integral value of every region (z_i to z_(i+1))
# note that it is UNNORMALIZED
sk <- function(x,my_points,my_values,my_slopes,my_domains) {
  n <- length(my_domains)-1
  integrals <- rep(NaN, n)
  for (i in seq(n))
  {
    a  <- my_domains[i]
    b  <- my_domains[i+1]
    xi <- my_points[i]
    yi <- my_values[i]
    k  <- my_slopes[i] 
    integrals[i] <- ((exp(yi + (k * (b - xi))) - exp(yi + (k * (a - xi)))) / k)
  }
  return(integrals)
}

################    vectorized function   ################

sk_new <- function(x, xi, yi, k, my_domains) {
  n <- length(my_domains)-1
  integrals <- ((exp(yi + (k * (my_domains[2:(n+1)] - xi))) 
                 - exp(yi + (k * (my_domains[1:n] - xi)))) / k)
  return(integrals)
}

################          test           ################

all(sk(x,my_points,my_values,my_slopes,my_domains) == sk_new(x,my_points,my_values,my_slopes,my_domains))

################    original function   ################

# The lower piecewise function
lower_piecewise <- function(x, x_values, y_values, domains) {
  ux <- ((x < x_values[1]) | (x > x_values[length(x_values)])) * -100
  for (i in seq(length(x_values)-1)) {
    ux <- ux + ((x > x_values[i]) & (x <= x_values[i+1])) * 
      ((x_values[i+1] - x) * y_values[i] + 
         (x - x_values[i]) * y_values[i+1]) / 
      (x_values[i+1] - x_values[i])
  }
  return(ux)
}

################    vectorized function   ################

lower_piecewise_new <- function(x, x_values, y_values, domains) {
  ux <- ((x < x_values[1]) | (x > x_values[length(x_values)])) * -100
  n <- length(x_values)-1
  u <- ((x > x_values[1:n]) & (x <= x_values[2:(n+1)])) * 
    ((x_values[2:(n+1)] - x) * y_values[1:n] + 
       (x - x_values[1:n]) * y_values[2:(n+1)]) / 
    (x_values[2:(n+1)] - x_values[1:n])
  return(ux+sum(u))
}

################          test           ################

lower_piecewise(x, x_values, y_values, domains) == lower_piecewise_new(x, x_values, y_values, domains)
