# Final Project.

simulate_things <- function(g, my_total_range, n) {

sampled_values <- rep(NaN,n)
i <- 1

BIG_NEG_NUM <- -1e309
  
z <- seq(from=-5, to=5, by=.01)

initial_domain = c(-3,3)
library(mosaic)

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

upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
  fx <- 0
  for (i in seq(length(domains)-1)) {
    fx <- fx + 
      (x > domains[i] & x <= domains[i+1]) * 
      (((x - x_intercepts[i]) * slopes[i]) + y_values[i])
  }
  return(fx)
}

exp_upper_piecewise <- function(x, x_intercepts, y_values, slopes, domains) {
  fx <- 0
  for (i in seq(length(domains)-1)) {
    fx <- fx + 
      (x > domains[i] & x <= domains[i+1]) * 
      (((x - x_intercepts[i]) * slopes[i]) + y_values[i])
  }
  return(exp(fx))
}

lower_piecewise <- function(x, x_values, y_values, domains) {
  ux <- ((x < x_values[1]) | (x > x_values[length(x_values)])) * -100
  for (i in seq(length(my_points)-1)) {
       ux <- ux + ((x > x_values[i]) & (x <= x_values[i+1])) * 
         ((x_values[i+1] - x) * y_values[i] + 
       (x - x_values[i]) * y_values[i+1]) / 
       (x_values[i+1] - x_values[i])
  }
  return(ux)
}

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

dom <- seq(from=-5, to=5, by=0.01)

x <- seq(-100, 100, 0.001)

my_points <- c(x1,x2)
my_slopes <- dh(my_points)
my_values <- h(my_points)

total_range <- my_total_range

total_chosen <- 1

domains <- sort(c(total_range, get_zj(x_values = my_points, 
                                  y_values = my_values, 
                                  slopes = my_slopes)))

while (total_chosen <= n)
{

integral_values <- sk(x,my_points,my_values,my_slopes,domains)
normalized_integral_values <- integral_values / sum(integral_values)


cs_normalized_integral_values <- c(cumsum(normalized_integral_values))
total_integral_values <- integral_values

cu <- integral_values[length(integral_values)]

ur <- runif(min=0,max=1,n=1)

# ri is "region index"
ri <- min(which(cs_normalized_integral_values >= ur))
# random variable
ur <- runif(min=0,max=1,n=1)
# the domain
zi <- domains[ri]
s <- my_slopes[ri]
nrmlztn <- 1/integral_values[ri]
y <- my_values[ri]
xc <- my_points[ri]

xt <- log(((s * ur)/(nrmlztn * exp(y - s * xc))) + exp(s * zi)) / s

# print("Xt and inside")
# print(xt)
# print(((s * ur)/(nrmlztn * exp(y - s * xc))) + exp(s * zi))

fx <- upper_piecewise(x = xt,
                      x_intercepts = my_points, 
                      y_values = my_values,
                      slopes = my_slopes,
                      domains = domains)

ux <- lower_piecewise(x = xt,
                      x_values = my_points,
                      y_values = my_values,
                      domains = domains)


w <- runif(min = 0, max = 1, n = 1)

if (w <= (exp(ux - fx)))
{
  sampled_values[total_chosen] <- xt
  total_chosen <- total_chosen+1
}
else {
 if (w <= (exp(h(xt) - fx)))
 {
   sampled_values[total_chosen] <- xt
   total_chosen <- total_chosen+1
   
   if (length(my_points) < 100)
   {
     my_points <- sort(c(my_points, xt))
     my_slopes <- dh(my_points)
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
sv <- simulate_things(dnorm, c(-Inf,Inf), n = 10000)
print(length(sv))
# print(sv)
histogram(sv, col='blue', prob=TRUE, bw=.5)
x<- seq(0,4,.01)

