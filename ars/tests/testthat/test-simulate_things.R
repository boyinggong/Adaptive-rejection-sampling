context("Main sampling function")

set.seed(0)

test_that("special case are correct", {
  # test normal
  expect_true(ks.test(simulate_things(dnorm, c(-Inf,Inf), n = 10000), pnorm)$p.value > 0.05)
  
  # test Chi square (degree of freedom = 3)
  dchisq_df3 <- function(x) { return( dchisq(x,df=3) ) }
  pchisq_df3 <- function(x) { return( pchisq(x,df=3) ) }
  expect_true(ks.test(simulate_things(dchisq_df3, c(0,Inf), n = 10000), pchisq_df3)$p.value > 0.05)
  
  # test uniform
  expect_true(ks.test(simulate_things(dunif, c(0,1), n = 10000), punif)$p.value > 0.05)
  
  # test Exponential
  expect_true(ks.test(simulate_things(dexp, c(0,Inf), n = 10000), pexp)$p.value > 0.05)
  
  # test Beta
  dtst_beta <- function(x) { return( dbeta(x, 2, 2) ) }
  ptst_beta <- function(x) { return( dbeta(x, 2, 2) ) }
  expect_true(ks.test(simulate_things(dtst_beta, c(0,1), n = 10000), ptst_beta)$p.value > 0.05)
  
  # test tail truncated normal
  dtrctNmT <- function(x){
    if (abs(x) > 1) return(0)
    else return(dnorm(x)/(pnorm(1)-pnorm(-1)))
  }
  ptrctNmT <- function(x){
    if (x < -1) return(0)
    else if(x > 1) return(1)
    else return((pnorm(x)-pnorm(-1))/(pnorm(1)-pnorm(-1)))
  }
  expect_true(ks.test(simulate_things(dtrctNmT, c(-1,1), n = 10000), ptrctNmT)$p.value > 0.05)
  
  # test upper truncated normal
  dtrctNmU <- function(x){
    if (abs(x) < 1) return(dnorm(1)/(dnorm(1)*2+pnorm(-1)*2))
    else return(dnorm(x)/(dnorm(1)*2+pnorm(-1)*2))
  }
  ptrctNmU <- function(x){
    if (x < -1) return(pnorm(x)/(dnorm(1)*2+pnorm(-1)*2))
    else if(x > 1) return((pnorm(-1)+dnorm(-1)*2+(pnorm(x)-pnorm(1)*2))/(dnorm(1)*2+pnorm(-1)*2))
    else return((pnorm(-1)+dnorm(-1)*(x+1))/(dnorm(1)*2+pnorm(-1)*2))
  }
  expect_true(ks.test(simulate_things(dtrctNmU, c(-Inf,Inf), n = 10000), ptrctNmU)$p.value > 0.05)
})

test_that("throw error when density is not concave", {
  # test Beta
  dtst_beta <- function(x) { return( dbeta(x, 1/2, 2) ) }
  expect_error(simulate_things(dtst_beta, c(0,1), n = 10000), "density function is not concave")
  
  # test student t
  dtst_t <- function(x) { return( dt(x, df = 2) ) }
  expect_error(simulate_things(dtst_t, c(-Inf,Inf), n = 10000), "density function is not concave")
  
  # test F
  dtst_f <- function(x) { return( df(x, 3, 3) ) }
  expect_error(simulate_things(dtst_f, c(0,Inf), n = 10000), "density function is not concave")
})

test_that("throw error for other argument error", {
  expect_error(simulate_things(dnorm, c("c",Inf), n = 10000), "input range is not numeric")
  expect_error(simulate_things(dnorm, c(1, 2, 3), n = 10000), "input range length error")
  expect_error(simulate_things(dnorm, c(-Inf, Inf), n = -1), "sample size should be positive integer")
  expect_error(simulate_things(dnorm, c(-Inf, Inf), n = 10.5), "sample size should be positive integer")
})
