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
  expect_true(ks.test(ars(dchisq, c(1,Inf), n = 1000, df = 3), 
                      function(x) {ptrunc(x, "chisq", a = 1, b = Inf, df=3)})$p.value > 0.05)
  expect_true(ks.test(ars(dchisq, c(0,Inf), n = 1000, df = 3), 
                      function(x) {ptrunc(x, "chisq", a = 0, b = Inf, df=3)})$p.value > 0.05)
  expect_true(ks.test(ars(dchisq, c(1,3), n = 1000, df = 3), 
                      function(x) {ptrunc(x, "chisq", a = 1, b = 3, df=3)})$p.value > 0.05)
  
  print("test Beta and tail truncated Beta when df1 > 1, df2 > 1")
  expect_true(ks.test(ars(dbeta, c(0,1), n = 1000, shape1 = 2, shape2 = 2),
                      function(x) {pbeta(x, 2, 2)})$p.value > 0.05)
  expect_true(ks.test(ars(dbeta, c(0,1/2), n = 1000, shape1 = 2, shape2 = 2),
                      function(x) {ptrunc(x, "beta", a = 0, b = 1/2, 2, 2)})$p.value > 0.05)
  expect_true(ks.test(ars(dbeta, c(1/4,3/4), n = 1000, shape1 = 2, shape2 = 2), 
                      function(x) {ptrunc(x, "beta", a = 1/4, b = 3/4, 2, 2)})$p.value > 0.05)

  print("test uniform")
  expect_true(ks.test(ars(dunif, c(0,1), n = 10000), punif)$p.value > 0.05)
  
  print("test Exponential")
  expect_true(ks.test(ars(dexp, c(0,Inf), n = 10000), pexp)$p.value > 0.05)
  expect_true(ks.test(ars(dchisq, c(0,Inf), n = 1000, df = 2), 
                      function(x) {ptrunc(x, "chisq", a = 0, b = Inf, df=2)})$p.value > 0.05)  
  
})

test_that("throw error when density is not concave", {
  cat("\nthrow error when density is not concave\n")
  print("test student t")
  expect_error(ars(dt, c(-Inf,Inf), n = 1000, df = 2), 
               "Please check the log-concavity of probability density function")
  expect_error(ars(dt, c(-Inf,Inf), n = 1000, df = 5), 
               "Please check the log-concavity of probability density function")
  
  print("chi square, df = 1")
  expect_error(ars(dchisq, c(0,Inf), n = 1000, df = 1), 
               "Please check the log-concavity of probability density function")
  
  print("test F")
  expect_error(ars(df, c(0,Inf), n = 10000, df1 = 3, df2 = 3), 
               "Please check the log-concavity of probability density function")
})

test_that("throw error when inputs error", {
  expect_error(ars(dnorm, c("c",Inf), n = 10000), "Input range is not numeric")
  expect_error(ars(dnorm, c(1, 2, 3), n = 10000), "Input range length error")
  expect_error(ars(dnorm, c(-Inf, Inf), n = -1), "Sample size should be positive integer")
  expect_error(ars(dnorm, c(-Inf, Inf), n = 10.5), "Sample size should be positive integer")
  expect_error(ars(dnorm, c(3, 3), n = 10), "Invalid support")
})
