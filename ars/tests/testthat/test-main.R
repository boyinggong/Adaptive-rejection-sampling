context("Main sampling function")

set.seed(2015243)

test_that("special cases are correct", {
  cat("\nspecial cases are correct\n")
  # probability distribution function of truncated distributions
  ptrunc <- function(x, spec, a = -Inf, b = Inf, ...)
  {
    tt <- x
    aa <- rep(a, length(x))
    bb <- rep(b, length(x))
    G <- get(paste("p", spec, sep = ""), mode = "function")
    tt <- G(apply(cbind(apply(cbind(x, bb), 1, min), aa), 1, max), ...)
    tt <- tt - G(aa, ...)
    tt <- tt/(G(bb, ...) - G(aa, ...))
    return(tt)
  }
  
  print("test normal and tail truncated normal")
  expect_true(ks.test(ars(dnorm, c(-Inf,Inf), n = 10000), pnorm)$p.value > 0.05)
  expect_true(ks.test(ars(dnorm, c(-1,1), n = 1000), 
                      function(x){ptrunc(x, "norm", a = -1, b = 1)})$p.value > 0.05)
  expect_true(ks.test(ars(dnorm, c(-Inf,1), n = 1000), 
                      function(x){ptrunc(x, "norm", a = -Inf, b = 1)})$p.value > 0.05)
  expect_true(ks.test(ars(dnorm, c(-Inf,-1), n = 1000), 
                      function(x){ptrunc(x, "norm", a = -Inf, b = -1)})$p.value > 0.05)
  expect_true(ks.test(ars(dnorm, c(-1,Inf), n = 1000), 
                      function(x){ptrunc(x, "norm", a = -1, b = Inf)})$p.value > 0.05)
  expect_true(ks.test(ars(dnorm, c(1,Inf), n = 1000), 
                      function(x){ptrunc(x, "norm", a = 1, b = Inf)})$p.value > 0.05)
  
  print("test Chi square and tail truncated Chi square when df > 2")
  dchisq_df3 <- function(x) { return( dchisq(x,df=3) ) }
  expect_true(ks.test(ars(dchisq_df3, c(1,Inf), n = 1000), 
                      function(x) {ptrunc(x, "chisq", a = 1, b = Inf, df=3)})$p.value > 0.05)
  expect_true(ks.test(ars(dchisq_df3, c(0,Inf), n = 1000), 
                      function(x) {ptrunc(x, "chisq", a = 0, b = Inf, df=3)})$p.value > 0.05)
  expect_true(ks.test(ars(dchisq_df3, c(1,3), n = 1000), 
                      function(x) {ptrunc(x, "chisq", a = 1, b = 3, df=3)})$p.value > 0.05)
  
  print("test Beta and tail truncated Beta when df1 > 1, df2 > 1")
  dtst_beta <- function(x) {dbeta(x, 2, 2)}
  expect_true(ks.test(ars(dtst_beta, c(0,1), n = 1000),
                      function(x) {pbeta(x, 2, 2)})$p.value > 0.05)
  expect_true(ks.test(ars(dtst_beta, c(0,1/2), n = 1000),
                      function(x) {ptrunc(x, "beta", a = 0, b = 1/2, 2, 2)})$p.value > 0.05)
  expect_true(ks.test(ars(dtst_beta, c(1/4,3/4), n = 1000), 
                      function(x) {ptrunc(x, "beta", a = 1/4, b = 3/4, 2, 2)})$p.value > 0.05)

  print("test uniform")
  expect_true(ks.test(ars(dunif, c(0,1), n = 10000), punif)$p.value > 0.05)
  
  print("test Exponential")
  expect_true(ks.test(ars(dexp, c(0,Inf), n = 10000), pexp)$p.value > 0.05)
  dchisq_df2 <- function(x) { return( dchisq(x,df=2) ) }
  expect_true(ks.test(ars(dchisq_df2, c(0,Inf), n = 1000), 
                      function(x) {ptrunc(x, "chisq", a = 0, b = Inf, df=2)})$p.value > 0.05)  
  
  
#   print("test upper truncated normal")
#   dtrctNmU <- function(x){
#     if (abs(x) < 1) return(dnorm(1)/(dnorm(1)*2+pnorm(-1)*2))
#     else return(dnorm(x)/(dnorm(1)*2+pnorm(-1)*2))
#   }
#   ptrctNmU <- function(x){
#     if (x < -1) return(pnorm(x)/(dnorm(1)*2+pnorm(-1)*2))
#     else if(x > 1) return((pnorm(-1)+dnorm(-1)*2+(pnorm(x)-pnorm(1)*2))/(dnorm(1)*2+pnorm(-1)*2))
#     else return((pnorm(-1)+dnorm(-1)*(x+1))/(dnorm(1)*2+pnorm(-1)*2))
#   }
#   expect_true(ks.test(ars(dtrctNmU, c(-Inf,Inf), n = 10000), ptrctNmU)$p.value > 0.05)
})

test_that("throw error when density is not concave", {
  cat("\nthrow error when density is not concave\n")
  print("test student t")
  dtst_t2 <- function(x) { return( dt(x, df = 2) ) }
  expect_error(ars(dtst_t2, c(-Inf,Inf), n = 1000), 
               "Please check the log-concavity of probability density function")
  dtst_t5 <- function(x) { return( dt(x, df = 5) ) }
  expect_error(ars(dtst_t5, c(-Inf,Inf), n = 1000), 
               "Please check the log-concavity of probability density function")
  
  print("chi square, df = 1")
  dchisq_df1 <- function(x) { return( dchisq(x,df=1) ) }
  expect_error(ars(dchisq_df1, c(0,Inf), n = 1000), 
               "Please check the log-concavity of probability density function")
  
  print("test F")
  dtst_f33 <- function(x) { return( df(x, 3, 3) ) }
  expect_error(ars(dtst_f33, c(0,Inf), n = 10000), 
               "Please check the log-concavity of probability density function")
})

test_that("throw error when inputs error", {
  expect_error(ars(dnorm, c("c",Inf), n = 10000), "input range is not numeric")
  expect_error(ars(dnorm, c(1, 2, 3), n = 10000), "input range length error")
  expect_error(ars(dnorm, c(-Inf, Inf), n = -1), "sample size should be positive integer")
  expect_error(ars(dnorm, c(-Inf, Inf), n = 10.5), "sample size should be positive integer")
})
