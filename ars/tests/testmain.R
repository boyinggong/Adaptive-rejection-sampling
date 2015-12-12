setwd('~/ars/R/simulate_things.R')

check_den <- function(x,x_ars) {
  plot(density(x),type = 'l',col = "blue", xlab = "x")
  lines(density(x_ars),type = 'l', col = "red")
}

# number of observations
n <- 200

## exponential distribution
x <- rexp(n)
x_ars <- simulate_things(dexp,c(0,500),n)
check_den(x,x_ars)


## Uniform distribution
x <- runif(-10,10)
x_ars <- simulate_things(dunif,(-10,10),n)
check_den(x,x_ars)


## Chi-square with df 1
# not log-concave
chisq1 <- function(x) dchisq(x,1)
x_ars <- simulate_things(chisq1,c(0.0001,100),n)

## Chi-square with df > 1
chisq5 <- function(x) dchisq(x,5)
x <- rchisq(n,5)
x_ars <- simulate_things(chisq5,c(0.0001,100),n)
check_den(x,x_ars)


## Laplace (double exponential distribution)
install.packages("smoothmest")
library(smoothmest)

laplace <- function(x) ddoublex(x, mu = 0, lambda = 1)
x <- rdoublex(n, mu = 0, lambda = 1)
x_ars <- simulate_things(laplace,c(-5,5),n)
check_den(x,x_ars)