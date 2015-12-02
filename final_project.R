# Final Project.

simulate_things <- function(g, n) {

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

total_range <- c(-Inf, Inf)

total_chosen <- 1

domains <- sort(c(total_range, get_zj(x_values = my_points, 
                                  y_values = my_values, 
                                  slopes = my_slopes)))

while (total_chosen <= n)
{
#fx <- upper_piecewise(x = x,
#                      x_intercepts = my_points, 
#                      y_values = my_values,
#                      slopes = my_slopes,
#                      domains = domains)

#ux <- lower_piecewise(x = x,
#             x_values = my_points,
#             y_values = my_values,
#             domains = domains)

# percentages <- rep(0, length(z_points)+1)



integral_values <- sk(x,my_points,my_values,my_slopes,domains)
normalized_integral_values <- integral_values / sum(integral_values)

# print(normalized_integral_values)
# print(sum(normalized_integral_values))

cs_normalized_integral_values <- c(0,cumsum(normalized_integral_values))
# cs_normalized_integral_values <- c(0, cumsum(normalized_integral_values))
total_integral_values <- integral_values

cu <- integral_values[length(integral_values)]

ur <- runif(min=0,max=1,n=1)

# ri is "region index"
ri <- min(which(cs_normalized_integral_values <= ur))

ur <- runif(min=0,max=1,n=1)

print("whoa")
print(length(domains))
print(domains)
print(length(total_integral_values))

zi <- domains[ri]

xt <- log(1 + ((my_slopes[ri] * cu *
                      (ur - cs_normalized_integral_values[ri])) / 
                      (exp(my_values[ri])))) / my_slopes[ri]

print(xt)
print((1 + ((my_slopes[ri] * cu *
               (ur - cs_normalized_integral_values[ri])) / 
              (exp(my_values[ri])))))

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
# print(my_points)

# Plot
# plot(x = x, y = log(dnorm(x)), type="l",ylim=c(-8,3), xlim=c(-4, 4))
# points(x = x, y = fx, type="l", ylim=c(-5,3))
# points(x = x, y = ux, type="l", ylim=c(-5,3))

return(sampled_values)
}
sv <- simulate_things(dnorm, n = 1000)
print(length(sv))
# print(sv)
histogram(exp(sv))
